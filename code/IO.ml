open Core.Std
open Printf
open Types

let _debug = false

module type IO =
sig

  type output_destination
  type input_destination

  val open_output_destination : string -> output_destination

  val close_output_destination : output_destination -> unit

  val open_input_destination : string ->  input_destination

  val close_input_destination : input_destination -> unit

  val write_line : output_destination -> string -> unit

  val write_lines : output_destination -> string list -> unit

  val read_line : input_destination -> string option

  val read_lines : input_destination -> string list

  val candle_of_line : string -> candle option

  val count_lines : filename -> int

end

module File_IO : IO =
struct

  type input_destination = in_channel
  type output_destination = out_channel

  let open_output_destination (filename:string) : output_destination =
    try
      open_out filename
    with
    | Not_found ->  Printf.printf "File Not Found..%s\n" filename;
                    raise Not_found
    | e -> Printf.printf "Exception while opening the file..%s\n" filename;
           raise e;
  ;;

  let close_output_destination (od:output_destination) =
    close_out_noerr od;
  ;;

  let open_input_destination (filename:string) : input_destination =
    try
      open_in filename
    with
    | Not_found -> Printf.printf "File Not Found..%s\n" filename;
                   raise Not_found
    | e -> Printf.printf "Exception while opening the file..%s\n" filename;
           raise e;
  ;;

  let close_input_destination (id:input_destination) =
    close_in_noerr id;
  ;;

  let write_line (od:output_destination) (line:string) =
    Out_channel.output_lines od (line::[])
  ;;

  let write_lines (od:output_destination) (lines: string list) =
    Out_channel.output_lines od lines
  ;;

  let read_line (id:input_destination) : string option =
    In_channel.input_line id
  ;;

  let read_lines (id:input_destination) : string list =
    In_channel.input_lines id
  ;;

  exception Invalid_Input

  let count_lines (source:filename) : int =
    let id = open_input_destination source in
    let count = ref 0 in
    let loop = ref true in
    (try while !loop do
      match read_line id with
      | None -> loop := false
      | Some x -> inc count
    done;
    with Sys_error _ -> 
      close_input_destination id);
    !count

  let candle_of_line (line : string) : candle option =
     let row = String.split line ',' in
    if List.length row < 6 then begin
      Printf.printf
        "[WARNING!] Encountered a CSV row with unsupported data.\n";
      None end
    else
      let column (n:int) : float =
        match (List.nth row n) with
        | None -> raise Invalid_Input
        | Some x ->
            let fl = Float.of_string x in
            (*Printf.printf "%s " x;*) fl
      in
      try
        let c =
        {t=0.;
         o=column 1;
         h=column 2;
         l=column 3;
         c=column 4;
         v=column 5;}
        in
        if _debug then
          Printf.printf "candle_of_line: result: %s" (string_of_candle c);
        Some c
      with Invalid_argument _ | Invalid_Input -> None
    ;;

end
