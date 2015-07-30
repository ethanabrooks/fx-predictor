open Core.Std
open Csv
open Types
open DataRecord
open Statistics

exception TODO

let _debug = false

type attribute = {category:string;count:int}

(*///////////////////////////////////////////////////////////////////////////*)
(*///////////////////////////////////////////////////////////////////////////*)
(*****************************************************************************)

(* Interface for Naive Bayes data model. This is the data structure which
 * stores necessary values needed for prediction computations. It is created
 * by feeding it data records of attributes and values, one by one. Once all
 * the data records have been input to this model, the model finalizes itself 
 * for usage and storage. The model can be stored in a file on the disk and 
 * can also rebuild itself from the file so that we don't have the re-create
 * the trained model again and again if the training data remains the same *)  
class type nb_model_t =
object

  (*The methods adds a new data record to the model. The model updates
   *its internal counts after the addition of each record *) 
  method addDataRecord : Data.record -> unit

  (*This method should be called once all the records have been added.
   *It calculates the various probabilities based on the attribute/category
   *counts and gets ready for usage as well as storage *) 
  method finalize : unit -> unit

  (*Converts the model to text so that it can be stored to disk *)
  method convert_to_text : string list list

  (*Loads itself from the text representation *)
  method load_from_text : string list list -> unit

  (*Stores itself to a file *) 
  method store_in_file : string -> unit

  (*Loads itself from a file *)
  method load_from_file : string -> unit

  (*The log of a category's prior probability *)
  method log_cat_prior_prob : Data.category -> float

  (*The logs of conditional probabilities of all the attribute values within 
   * a data record given a category *)
  method log_cat_cond_prob : Data.record -> Data.category -> float list

  (*Find out the category of a data record *)
  method categorize : Data.record -> string -> unit

  (*Check attributes of a data record  within the hash table *)
  method check_attributes : Data.record -> string -> unit

  (*Finalize the attribute values in the hash tables after all the 
   *data records have been input to the model *)
  method finalize_attrib_tbl : unit

  (*Convert the attribute hash table to string representaion so that it can
   *be stored *)
  method convert_attrib_tbl : string list list

  (*Re-create the attribute hash table from its string list representation *)
  method load_attrib_tbl : string list -> unit

end

(* This class implements the nb_model to provide a model for discrete attribute
 * data *)
class nb_model : nb_model_t =
object(self)
  (* Hash table to store attribute counts for a category *)
  val mutable attrib_cat_counts_tbl =  String.Table.create()
  (* Hash table to store category counts *)
  val mutable cat_counts_tbl =  String.Table.create()
  (* Hash table to store logarithm probabilities of category counts *)
  val mutable cat_logs_tbl = String.Table.create()
  (* Hash table to store logarithm probabilities of attribute counts for cat *) 
  val mutable attrib_cat_logs_tbl = String.Table.create()
  (* Total number of records input to the model *)
  val mutable total_records : int = 0
  val mutable str_list : string list = []
  (****)

  (*Adds a new data record to the model. It assigns the data  
   *record a category, updates internal category counts and also updates
   *the internal counts for the atributes in the data record *) 
  method addDataRecord (dr:Data.record) =
    total_records <- total_records+1;
    let category = Data.category dr in
    self#categorize dr category;
    self#check_attributes dr category;

  (* Updates the category counts in the hash table *)
  method categorize (dr:Data.record) (key:string) : unit =
      match Hashtbl.find cat_counts_tbl key with
      | Some x -> Hashtbl.set cat_counts_tbl key (x+1)
      | None -> Hashtbl.set cat_counts_tbl key 1; 

  (* Updates the attribute counts for a category within the hash table *)
  method check_attributes (dr:Data.record) (category:string) 
  : unit =
    let attribute_values = Data.d_attributes dr in
    List.iter attribute_values ~f:(fun (x:d_id) ->
      let (attrib_name,attrib_value) = x in
      let key = attrib_name^"@"^attrib_value^"@"^category in
      match Hashtbl.find attrib_cat_counts_tbl key with
      | Some x -> Hashtbl.set attrib_cat_counts_tbl key
        {category=category;count=x.count+1}
      | None -> Hashtbl.set attrib_cat_counts_tbl key 
        {category=category;count=1})

  (* Finalize attributes table and add logarithm probabilities for attributes *)
  method finalize_attrib_tbl =
    Hashtbl.iter attrib_cat_counts_tbl
      ~f:(fun ~key:k ~data:v ->
        match Hashtbl.find cat_counts_tbl v.category with
        | None -> failwith "can't find category in hash table"
        | Some c_count -> 
          let ratio = ((Int.to_float v.count) /. (Int.to_float c_count)) in
          let result = log ratio in
          Hashtbl.set attrib_cat_logs_tbl k result);

  (*Finalize the model data structure after all the data records have been 
   *input to the model *)
  method finalize () =
    Hashtbl.iter cat_counts_tbl
      ~f:(fun ~key:k ~data:v ->
         let ratio = ((Int.to_float v) /. (Int.to_float total_records)) in
         let result = log ratio in
         Hashtbl.set cat_logs_tbl k result);
         self#finalize_attrib_tbl;

  (*The log of a category's prior probability *)
  method log_cat_prior_prob (category:Data.category) : float =
    let key = Data.category_to_string category in
    match Hashtbl.find cat_logs_tbl key with
    | None -> 0.
    | Some x -> x

  (*The logs of conditional probabilities of all the attribute values within 
   * a data record given a category *)
  method log_cat_cond_prob (dr:Data.record) (category:Data.category)
  : float list =
    let attrib_list = Data.d_attributes dr in
    List.map attrib_list 
      ~f:(fun attrib -> 
        let name, value = attrib in
        let cat_name = Data.category_to_string category in
        let key = name^"@"^value^"@"^cat_name in
          if _debug then Printf.printf "
          key queried: %s" key;
        match Hashtbl.find attrib_cat_logs_tbl key with
        | None -> 
            if _debug then Printf.printf " nothing found :(\n";
            0.
        | Some x -> x);

  (*Convert the attribute hash table to string representaion so that it can
   *be stored *)
  method convert_attrib_tbl =
    Hashtbl.fold attrib_cat_logs_tbl
      ~init:[]
      ~f:(fun ~key:k ~data:v acc ->
        (["ATT";k;(Float.to_string v)])::acc)

  (*Converts the model to text so that it can be stored to disk *)
  method convert_to_text : string list list =
    let str_list=Hashtbl.fold
      cat_logs_tbl
      ~init:self#convert_attrib_tbl
      ~f:(fun ~key:k ~data:v acc ->
        (["CAT";k;(Float.to_string v)])::acc) in
    ["TOT";(Int.to_string total_records)]::str_list;

  (*Stores itself to a file 
   *Using the open source Cvs module from:
   *https://github.com/Chris00/ocaml-csv/blob/master/src/csv.ml *) 
  method store_in_file (filename: string) =
    let str_list=self#convert_to_text in
    let _ =
      try
        Unix.unlink filename
      with Unix.Unix_error (_,_,_) ->
        ()
    in
    let _ = Csv.save ~separator:':' filename str_list in
    ();

  (*Re-create the attribute hash table from its string list representation *)
  method load_attrib_tbl (x:string list) =
    let key = x$1 in
    if _debug then Printf.printf "
    key loaded in hash table: %s" key;
    Hashtbl.set attrib_cat_logs_tbl
      key (Float.of_string (x$2));

  (*Loads itself from text *) 
  method load_from_text (str_list : string list list) : unit =
    List.iter str_list
    ~f:(fun x ->
        let rec_type = x$0 in
        if rec_type="TOT" then
          total_records <- int_of_string(x$1)
        else if rec_type="CAT" then
          Hashtbl.set cat_logs_tbl
            (x$1)
            (Float.of_string (x$2))
        else if rec_type="ATT" then
          self#load_attrib_tbl x);
          if _debug then Printf.printf "\nloaded Hash table:\n%s\n"
          (concat_n (List.concat self#convert_to_text))

  (*Loads itself from a file 
   *Using the open source Cvs module from:
   *https://github.com/Chris00/ocaml-csv/blob/master/src/csv.ml *) 
  method load_from_file (filename: string) : unit =
    let row_list = Csv.load ~separator:':' filename in
    self#load_from_text row_list;

end

(* This class extends the nb_model class to provide a model for continuous 
 * attribute data *)
class nb_model_c : nb_model_t =
object(self)
  inherit nb_model as super

  val mutable total_records : int = 0
  val mutable attrib_cat_counts_tbl =  String.Table.create()
  val mutable attrib_cat_logs_tbl = String.Table.create()
  val mutable cat_counts_tbl =  String.Table.create()
  val mutable cat_logs_tbl = String.Table.create()
  (****)

  (* Updates the attribute counts for a category within the hash table. 
   * This method is called from the addDataRecord method in the super 
   * class *)
  method check_attributes (dr:Data.record) (category:string) : unit =
    let attribute_values = Data.c_attributes dr in
    List.iter attribute_values ~f:(fun (name,value:c_id) ->
      let key = name ^ "@" ^ category in
      match Hashtbl.find attrib_cat_counts_tbl key with
      | Some calculator -> 
          if _debug then Printf.printf "
          value %s
          " category;
          calculator#add value
      | None ->
          if _debug then Printf.printf "
          value %s
          " category;
          Hashtbl.set attrib_cat_counts_tbl key 
               (new Statistics.calculator(value)));

  method finalize_attrib_tbl =
    Hashtbl.iter attrib_cat_counts_tbl
     ~f:(fun ~key:k ~data:calc ->
       let mean,var = calc#get_mean,calc#get_var in
       if _debug then
         Printf.printf "\n
         Attribute: %s
         Mean: %f
         Var: %f"
         k  calc#get_mean calc#get_var;
       Hashtbl.set attrib_cat_logs_tbl k (mean,var));

  method log_cat_cond_prob (dr:Data.record) (category:Data.category)
  : float list =
    let attrib_list = Data.c_attributes dr in
    let cat_name = Data.category_to_string category in
      List.map
      ~f:(fun attribute ->
        let attrib_name,attrib_value = attribute in
        let key = attrib_name ^ "@" ^ cat_name in
        match Hashtbl.find attrib_cat_logs_tbl key with
        | None -> 0.
        | Some (mean,var) -> 
            let probability = pdf mean var attrib_value in
            if _debug then 
              Printf.printf "Attribute probability: %f" probability;
            log probability)
      attrib_list

  method convert_attrib_tbl =
    Hashtbl.fold attrib_cat_logs_tbl
      ~init:[]
      ~f:(fun ~key:k ~data:v acc ->
        let mean,var = v in
        (["ATT";k;
        (Float.to_string mean);(Float.to_string var)])::acc)

  method load_attrib_tbl (x:string list) =
    let key = x$1 in
    Hashtbl.set attrib_cat_logs_tbl
      key ((Float.of_string (x$2)),(Float.of_string (x$3)));

end
