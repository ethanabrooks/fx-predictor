open Core.Std
open Unix

open Predict
open Types
open NBModel
open DataRecord
open Statistics
(****)

let _debug = false

(*///////////////////////////////////////////////////////////////////////////*)
(*///////////////////////////////////////////////////////////////////////////*)
(*****************************************************************************)

(* Takes currency labels and a filename, and processes the CSV currency data
  from said file into various more usable data structures... which are
  then used to generate "training data" that is saved to disk as a new CSV
  file inside the "./data/" sub-directory. *)

let trainWithData (currencyA : string) (currencyB : string) (filename : string)
    (d_or_c : char) : unit =
    (* NOTE: Assumes `pre-sterilized` (to only alphanumeric) A/B labels. *)
  let model =
    if d_or_c = 'D' then
      new NBModel.nb_model
    else new NBModel.nb_model_c
  in
  Printf.printf "Training...\n";
  Data.iterate filename (fun dr -> model#addDataRecord dr);
  model#finalize ();
  Printf.printf "\n\n## Algorithm trained. \
    Writing data to './data/' directory.\n";
  try
    Unix.mkdir "./data"
  with Unix.Unix_error (_,_,_) ->
    ();
  let filenameTrained = "./data/" ^ currencyA ^ "-" ^ currencyB ^ ".csv" in
  model#store_in_file filenameTrained;
  let fileStats = Unix.stat filenameTrained in
  Printf.printf "\n## Training data saved to: %s (%Ld bytes)\n"
      filenameTrained fileStats.Unix.st_size;
  exit 0
;;

(* Attempts to load cached currency "training data" for said currency labels.
  Applies said data to provided CSV currency data and outputs various
  statistics as well as a prediction for its "UP/DOWN" future. *)

let predictCurrency (currencyA : string) (currencyB : string)
  (filename : string) (d_or_c : char) =
  (* NOTE: Assumes `pre-sterilized` (to only alphanumeric) A/B labels. *)
  Printf.printf "## Loading training data...\n";
  let model =
    if d_or_c = 'D' then
      new NBModel.nb_model
    else new NBModel.nb_model_c
  in
  let filenameTrained = "./data/" ^ currencyA ^ "-" ^ currencyB ^ ".csv" in
  model#load_from_file filenameTrained;
  Printf.printf "## Training data file loaded!\n";
  let input_data : Data.record =
    match Data.of_file filename with
    | None -> failwith "[FAILURE] input cannot be processed";
    | Some record -> record
  in
  Printf.printf "## Analyzing data...\n";
  let probabilities = Predict.probabilities model input_data in
  let top_probability =
    match List.hd (List.rev probabilities) with
    | None -> failwith "[FAILURE] probabilities list is empty!"
    | Some (p,_) -> Data.category_to_string p
  in
  let rec print_probabilities probabilities =
    match probabilities with
    | [] -> print_string "\n"
    | (category, probability)::tl ->
        let percent =  probability *. 100. in
        Printf.printf "   %-8s\t%f\n" (Data.category_to_string category)
        percent; print_probabilities tl
  in
  Printf.printf "\n   Category\tProbability";
  Printf.printf "\n   ________\t___________\n";
  print_probabilities probabilities;
  Printf.printf "\n   The next direction is most likely: %s\n\n"
    top_probability
;;

let assessAccuracy  (currencyA : string) (currencyB : string)
  (filename : string) (d_or_c : char): unit =
  Printf.printf "## Loading training data...\n";
  let model =
    if d_or_c = 'D' then
      new NBModel.nb_model
    else new NBModel.nb_model_c
  in
  let filenameTrained = "./data/" ^ currencyA ^ "-" ^ currencyB ^ ".csv" in
  model#load_from_file filenameTrained;
  Printf.printf "## Training data file loaded!\n\
  ## Generating predictions...\n";
  let stat = new Statistics.calculator 0. in
  Data.iterate filename (fun dr -> stat#add (Predict.accuracy model dr));
  Printf.printf "\n\n## Predicted %f percent of data accurately.\n" 
  stat#get_mean;
;;




(*///////////////////////////////////////////////////////////////////////////*)
(*///////////////////////////////////////////////////////////////////////////*)
(*****************************************************************************)

(* Removes non-alphabetic characters from a string. - Sourced from:
  http://www.codecodex.com/wiki/Remove_non-letters_from_a_string#OCaml *)

let remove_nonalpha = Str.global_replace (Str.regexp "[^a-zA-Z]+") "";;

(* Parses command-line arguments and calls appropriate functions with said
  arguments as variables. Sterilizes them as necessary. *)

let run () : unit =
  let usage () =
      Printf.printf "usage: %s [train|predict|assess]\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv < 2 then
    usage()
  else
    match Sys.argv.(1) with
    | "train" | "predict" | "assess" ->
      (* "train" = Generates a `training data` CSV file from a candle CSV for
        future use.*)
      (* "predict" = Generates `prediction data` CSV file based on`training
          data` obtained from the pregenerated CSV files for this currency. *)
      (
      if Array.length Sys.argv <> 6 then
        (
        Printf.printf "usage: %s %s {[D]iscrete or [C]ontinuous 
          {currency A (ex. JPY)} {currency B (ex. USD)} 
          {*.csv filename for candle data}\n"
          Sys.argv.(0) Sys.argv.(1);
        exit 1
        )
      else
        let d_or_c = Char.uppercase
          (String.get (remove_nonalpha Sys.argv.(2)) 0) in
        let currencyA = String.uppercase
          (String.sub (remove_nonalpha Sys.argv.(3)) 0 3) in
        let currencyB = String.uppercase
          (String.sub (remove_nonalpha Sys.argv.(4)) 0 3) in
        if d_or_c <> 'D' && d_or_c <> 'C' then
          let _ = Printf.printf
            "Error: Please specify [D]iscrete or [C]ontinuous.\n" in
            exit 1
        else if currencyA = currencyB
            || String.length currencyA = 0
            || String.length currencyB = 0 then
          let _ = Printf.printf
            "Error: The currency labels that you provided are the same, \
                or invalid.\n" in
            exit 1
        else
          match Sys.argv.(1) with
          | "train" -> trainWithData currencyA currencyB Sys.argv.(5) d_or_c
          | "predict"-> predictCurrency currencyA currencyB Sys.argv.(5) d_or_c
          | "assess" -> assessAccuracy currencyA currencyB Sys.argv.(5) d_or_c
          | _ -> exit 1
      )
    | _ -> usage ()
;;


(*///////////////////////////////////////////////////////////////////////////*)
(*///////////////////////////////////////////////////////////////////////////*)
(*****************************************************************************)

try
  run () 
with
|e -> Printf.printf "Unexpected exception occured while running the command..\n"
;;
