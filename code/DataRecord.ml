open Types
open Core.Std
open IO
open Unix

exception TODO

let _debug = false

module type DATA =
  sig

    type record

    type category

    (* This is how we will use this function:
     * iterate_function [training data file name]
     * (fun dr -> nb_model#addDataRecord)
     * This will take the training data, break it into data records using
     * the sliding window methodology, and apply the given function to
     * every data record.*)
    val iterate : filename -> (record -> unit) -> unit

    (* We will use this function on the file that the user inputs as an
     * argument to the "predict" command. This is the path that the user is
     * trying to make predictions about. Let's say record_length is 5
     * and the user inputs a file with 9 rows (and therefore 9 candles). This
     * function should peel off the first five candles, convert them into a
     * data structure, and discard the rest. We can then break this
     * record down into attributes for use in the Predict module. *)
    val of_file : filename -> record option

    (* returns a string representation of the data record (for debugging) *)
    val to_string : record -> string

    (* gets continuous attributes for given record *)
    val c_attributes : record -> c_id list

    (* gets discrete attributes for given record *)
    val d_attributes : record -> d_id list

    (* category of record *)
    val category : record -> string

    (*all categories in a list *)
    val category_list : category list

    (* string represenation of category *)
    val category_to_string : category -> string

  end

module Data : DATA =
  struct

    (* Stores all data regarding inputs required for assigning categories or
     * attributes. For example, categories are assigned exclusively by the
     * trend (Up/Down/Equal) of the "future" candle *)
    type record = {path:path; future:candle; volume:float}

    (* the index of the "future" candle *)
    let outlook = 5

    (* data record length--categories/attributes may depend on this *)
    let path_length = 4

    let to_string (dr:record) =
      let rec candles_to_string (candles: candle list) : string =
        concat (List.map candles string_of_candle)
      in
      "path=" ^ (candles_to_string dr.path) ^
      "future_candle=" ^ (string_of_candle dr.future)

    (* If recurse is false, pulls lines from a given input_destination and
     * until a data record can be constructed and then applies f to it.
     * If recurse is true, iterates through all data records contained in the
     * input_destination applying f to each. *)
    let read_lines id (f:record -> unit) (recurse:bool) (load_bar_scalar:int)
    : unit =
      let count = ref 0 in
      let rec read_line  (c_list:candle list) (volume:float) (length:int) 
      (i:int) : unit =
        if recurse && i % load_bar_scalar = 0 then 
          (*print load bar*) Printf.printf "#"; flush_all();
        let line = File_IO.read_line id in
        match line with
        | None -> count:=i
        | Some x ->
            match File_IO.candle_of_line x with
            | None -> read_line c_list volume length i 
            | Some candle ->
               if length < outlook 
               then((* if path is too small to construct a record *)
                 if _debug then Printf.printf 
                   "\npath too small for record";
                 read_line (candle::c_list) (candle.v +. volume) (length+1) 
                   (i+1))
               else
                 let candles = List.rev (candle::c_list) in
                 if _debug then Printf.printf 
                   "candles converted to record: %s" 
                   (concat (List.map candles string_of_candle));
                 let record = {
                   path=keep path_length candles; 
                   future=candles$outlook; 
                   volume=volume} in
                 if _debug then Printf.printf "
                 record created: %s
                 " (to_string record);
                 f record; 
                 if recurse then
                   match candles with
                   | _::hd::_ -> read_line [hd] hd.v 1 (i+1)
                   | _ ->  count := i+1
      in
      read_line [] 0. 0 0;
      if recurse then Printf.printf "\n\n## %n lines read" !count
    ;;

    (* Applies function f to all records contained in given source *)
    let iterate (source:filename) (f:record -> unit) : unit =

      (* start load bar parameters *)
      let lines = File_IO.count_lines source in
      let load_bar_scalar = 95 in
      let load_bar_freq = max (lines/load_bar_scalar) 1 in
      let load_bar_length = lines/load_bar_freq+1 in
      print_times (min load_bar_length lines) "_";
      Printf.printf "\n";
      (* end load bar parameters *)

      let id = File_IO.open_input_destination source in
      ignore(File_IO.read_line id);
      read_lines id f true load_bar_freq;
      File_IO.close_input_destination id;
    ;;

    (* Tries to to pull a record from the first lines of given source *)
    let of_file (source:filename) : record option =
      let maybe_record = ref None in
      let id = File_IO.open_input_destination source in
      ignore(File_IO.read_line id);
      read_lines id (fun r -> maybe_record:=Some r) false 0;
      File_IO.close_input_destination id;
      !maybe_record
    ;;

    type category = Up | Down | Equal

    (* Helper function for determining candle trend *)
    let classify (v1:float) (v2:float) : category =
      if v2 -. v1 > 0. then Up
      else if v2 -. v1 < 0. then Down
      else Equal

    let category_to_string (cat:category) =
      match cat with
      | Up -> "UP"
      | Down -> "DOWN"
      | Equal -> "EQUAL"

    (* Implements methods for assigning categories to records. Modularized 
     * so that different schemes of categorization can be applied. *)
    module Category :
      sig
        type t
        val all : category list (* list of all categories *)
        val to_string : category -> string
        val get : record -> category
      end =
      struct
        type t = category
        let get (dr:record) = 
          let last = List.hd (List.rev dr.path) in
          match last with
          | None -> failwith "path is empty"
          | Some candle -> classify candle.c dr.future.c
        let all = [Up; Down; Equal]
        let to_string = category_to_string
      end

    let category_list = Category.all

    (* Implements methods for assigning attributes to records. Modularized
     * so that attributes can be added and removed without affecting the 
     * functionality of the rest of the program. *)
    module Continuous : 
    sig
      (* list of all functions that can evaluate a given
       * data record for attributes *)
      val functions : (record -> c_id list) list
    end =
      struct

        let map_path (prefix:string) (f:candle -> float) (dr:record) 
        : c_id list = 
          let rec map_path (p:path) (min:int) (ids:c_id list) : c_id list =
            match p with
            | [] -> ids
            | hd::tl -> 
                let attribute = prefix ^ (Int.to_string min) in
                let value = f hd in
                if _debug then
                  Printf.printf "\nattrib=%s val=%f\n" attribute value;
                map_path tl (min+1) ((attribute,value)::ids)
          in
          map_path dr.path 1 []
        (*let map_path  (f:candle -> c_id) (dr:record) = 
          List.map dr.path f*)

        let functions = [
          map_path "open" (fun c -> c.o);
          map_path "volume" (fun c -> c.v);
          (*map_path (fun c -> "low",c.l);
          map_path (fun c -> "close",c.c);*)
        ]

      end

    module Discrete :
    sig
      (* list of all functions that can evaluate a given
       * data record for attributes *)
      val functions : (record -> d_id list) list
    end =
      struct

        (* Assesses trend for each candle in the path field of a record *)
        let min_trend (dr:record) : d_id list  =
          if _debug then
            Printf.printf "data record passed to min_trend: %s
            path: %s
            future: %s
            volume: %f"
            (to_string dr) (concat (List.map dr.path string_of_candle))
            (string_of_candle dr.future) dr.volume;
          let rec min_trend (p:path) (min:int) (ids:d_id list)
          : d_id list  =
            match p with
            | [] -> ids
            | hd::tl ->
                let attribute = (Int.to_string min) ^ "-MIN-TREND" in
                let value = category_to_string (classify hd.o hd.c) in
                if _debug then
                  Printf.printf "\nattrib=%s val=%s\n" attribute value;
                min_trend tl (min+1) ((attribute,value)::ids)
          in 
          min_trend dr.path 1 []
        ;;

        (* categorizes the volume of inputs. Low is from 0 to mean.
         * Med is from mean to +1 standard deviation. 
         * Anything else is High *)
        let volume (dr:record) : d_id list =
          let value =
            if dr.volume < 5.*.Float.of_int path_length then "LOW"
            else if dr.volume < 55.*.Float.of_int path_length then "MED"
            else "HIGH"
          in [("VOLUME", value)]
        ;;

        let functions = [volume; min_trend]

      end

    (* Returns a list of (attribute name, attribute value) pairs
     * associated with given record *)
    let attributes (functions:(record -> 'a list) list) (dr:record)
    : 'a list =
      List.concat(List.map functions (fun f -> f dr))

    let c_attributes : record -> c_id list = 
      attributes Continuous.functions

    let d_attributes : record -> d_id list = 
      attributes Discrete.functions

    (* Returns category of given record *)
    let category (dr:record) : string = Category.to_string (Category.get dr)

  end
