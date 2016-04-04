open Utils

(* Fixed/periodic point functions *)

(*
evaluates the given function FUNC on input INPUT x number of times ( if x = 3, 
return f(f(f(input))) )
*)
let rec evaluate_function func input = function
| 0 -> input
| 1 -> func input
| x -> evaluate_function func (func input) (x - 1)

let rec computed_periodic_point equal func period initial =
let evaluated = evaluate_function func initial period in
if equal evaluated initial then initial else computed_periodic_point equal func 
period (func initial);;

let rec computed_fixed_point equal func initial =
  computed_periodic_point equal func 1 initial;;
