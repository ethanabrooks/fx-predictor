open Types

let _debug = false

class type stat_i =
  object
    method reset : unit
    method add : float -> unit
    method get_mean : float
    method get_var : float
  end

(* default settings for ivar, mean, and i *)
class calculator(initial_value): stat_i =
  object(self)

    initializer self#add initial_value

    val mutable i = 0.;
    val mutable ivar = 0.;
    val mutable mean = 0.;

    (* resets refs to defaults *)
    method reset : unit =
      if _debug then Printf.printf "\nresetting...\n";
      i <- 0.;
      ivar <- 0.;
      mean <- 0.;

    method add (x:float) : unit =
      i <- i +. 1.;
      ivar <- ivar +. ((i-. 1.)*.((x -. mean)**2.) /. i);
      mean <- mean +. ((x -. mean) /. i);
      if _debug then Printf.printf "
      ivar: %f
      mean: %f
      i: %f"
      ivar mean i;

    method get_mean : float = mean;

    method get_var : float = ivar /. i;

  end

let pi = 4.*.atan 1.;;

let pdf (mean:float) (var:float) (x:float) : float =
  if _debug then Printf.printf "pi: %f" pi;
  let exponent = -.((x -. mean)**2. /. (2.*.var)) in
  if _debug then Printf.printf "exponent: %f\n" exponent;
  let denominator = sqrt(2.*.var*.pi) in
  if _debug then Printf.printf "denominator: %f\n" denominator;
  exp exponent /. denominator;;

let () = 
  if _debug then Printf.printf "pdf: %f" (pdf 1. 4. 3.);
  assert(pdf 1. 4. 3. =~ 0.120985);
  assert(pdf 2. 1. 3. =~ 0.241970);
  assert(pdf 1. 4. 1. =~ 0.199471);
