open Random
open Core.Std

(* Time, Open, High, Low, Close, Volume *)
type candle = {t: float; o: float; h: float; l: float; c: float; v: float}
type path = candle list
type data_record = path  (* may be changed in the future *)
type c_id = string*float
type d_id = string*string
type filename = string

(* String concat adding \n at the end.
   Used in DataRecord.ml, Predict.ml and NBModel.ml *)
let rec concat string_list =
  match string_list with
  | [] -> "\n"
  | h::t -> h^","^(concat t)

(* String concat adding \n after each string. Used in NBModel.ml *)
let rec concat_n string_list =
  match string_list with
  | [] -> "\n"
  | h::t -> h^"\n"^(concat t)

(* String representation of candle type. Used in DataRecore.ml and IO.ml *)
let string_of_candle c =
  let string_list = List.map [c.t; c.o; c.h; c.l; c.c; c.v] Float.to_string in
  "("^(concat string_list)^")"

(* Return a string from float list. Used in NayveBayes.ml *)
let concat_floats float_list = concat (List.map float_list Float.to_string)

(* Used for testing *)
let string_of_d_id (id:d_id) =
  let a, b = id in
  a^":"^b

(* Used for testing *)
let string_of_c_id (id:c_id) =
  let a, b = id in
  a^":"^(Float.to_string b)

(* Used in Statistics.ml *)
let (++) (x:float ref) (delta:float) = x:=!x+.delta

let () = 
  let a = Random.float 100. in
  let b = Random.float 100. in
  let x = ref a in
  x++b;
  assert(!x=a+.b);
;;

(* Used in IO.ml *)
let inc (n:int ref) = n:=!n+1

let () = 
  let a = Random.int 100 in
  let x = ref a in
  inc x;
  assert(!x=a+1);
;;

(* Used in DataRecord.ml *)
let rec print_times (n:int) (text:string) : unit =
  if n <= 0 then () else (
    print_string text;
    print_times (n-1) text)

(* Used in DataRecord.ml *)
let keep (n:int) (alist:'a list) : 'a list =
  let rec keep (alist:'a list) (n:int) (kept:'a list)=
    if n <= 0 then List.rev kept else
      match alist with
      | [] -> raise (Invalid_argument (string_of_int n))
      | h::t -> keep t (n-1) (h::kept)
  in keep alist n []

let () = assert (keep 3 [2;3;5;67;5;6;7;8;64;4;6] = [2;3;5]);;
let () = assert (keep 0 [2;3;5;67;5;6;7;8;64;4;6] = []);;
let () = assert (try ignore(keep 5 [2;3;5]); false with _ -> true)

(* Used in NaiveBayes.ml. *)
let sum (flist:float list) : float = List.fold_left ~f:(+.) ~init:0. flist

let () = assert (sum (List.map [2;3;5;67;5;6;7;8;64;4;6] Float.of_int) = 177.);;
let () = assert (sum (List.map [0;0;0;5;6;7;8;64;4;6] Float.of_int) = 100.);;
let () = assert (sum (List.map [2;-3;5;-67;5;-6;7;8;64;4;6] Float.of_int) = 25.);;
let () = assert (sum (List.map [] Float.of_int) = 0.);;

(* Used in DataRecord.ml and NBModel.ml *)
let ($) (alist:'a list) (n:int) = List.nth_exn alist n

(* Used in NaiveBayes.ml *)
let rec zip (list1:'a list) (list2:'b list) : ('a*'b) list =
  match list1 with
  | [] -> []
  | h1::t1 -> 
      match list2 with
      | [] -> []
      | h2::t2 -> 
          (h1,h2)::(zip t1 t2)

(* used in Statistics.ml *)
let eps = 0.001
let (=~) (a:float) (b:float) = Float.abs (a-.b) < eps

let () = assert (1. =~ 1.0001)
let () = assert (1. =~ 0.99999)
(* ----------- Not in Use ---------------------------- *)

(* let rec drop (n:int) (alist:'a list) =
  if n <= 0 then alist else
    match alist with
    | [] -> raise (Invalid_argument (string_of_int n))
    | _::t -> drop (n-1) t

 let ($) (alist:'a list) (n:int) = List.nth alist n
 let ($$) (alist:'a list) (start:int) (stop:int) = keep stop (drop start alist)

 let eps = 0.0001
 let (=~) (a:float) (b:float) = abs_float (a-.b) < eps

 let remove x xlist =
  let rec remove x xlist rest =
    match xlist with
    | [] -> rest
    | h::t -> if h = x then t@rest else remove x t (h::rest)
  in remove x xlist [] *)
