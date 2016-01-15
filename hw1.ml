(* Utility functions *)

(* returns true if input contains 'obj' (semantic), else false*)
let rec contains obj = function
| [] -> false
| h :: t -> if h = obj then true else contains obj t;;

(* returns a list with ALL elements semantically equal to obj removed*)
let rec remove obj = function
| [] -> []
| h :: t -> if h = obj then remove obj t else h :: remove obj t;;

(* Set functions *)

(* returns true if a is a proper subset of b *)
let rec proper_subset a b =
match a with
| [] -> if b != [] then true else false
| h :: t -> if contains h b then proper_subset (remove h t) (remove h b) else false;;

(* returns true if a is equal to b (using set logic, where all elements of set are unique) *)
let rec equal_sets a b =
match a with
| [] -> if b = [] then true else false
| h :: t -> if contains h b then equal_sets (remove h t) (remove h b) else false;;

(* returns true if a is a (strict) subset of b *)
let rec subset a b =
match a with
| [] -> true
| _ -> if proper_subset a b || equal_sets a b then true else false;;

(* returns a list of all elements in a which are not also in b (set difference) *)
let rec set_diff a b = match a with
| [] -> []
| h :: t -> if contains h b then set_diff (remove h t) (remove h b) else h :: set_diff (remove h t) (remove h b);;

(* Fixed/periodic point functions *)

(*
evaluates the given function FUNC on input INPUT x number of times ( if x = 3, return f(f(f(input))) )
*)
let rec evaluate_function func input = function
| 0 -> input
| 1 -> func input
| x -> evaluate_function func (func input) (x - 1)

let rec computed_periodic_point equal func period initial =
let evaluated = evaluate_function func initial period in
if equal evaluated initial then initial else computed_periodic_point equal func period (func initial);;

let rec computed_fixed_point equal func initial =
  computed_periodic_point equal func 1 initial;;

(* Blind alley removal functions*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* returns true if all elements in rhs are terminals *)
let rec guaranteed_termination rhs =
match rhs with
| [] -> true
| N _ :: t -> false
| T _ :: t -> guaranteed_termination t
;;

(* returns list of all nonterminals which are guaranteed to terminate *)
let rec generate_white_list grammar =
match grammar with
| start, [] -> []
| start, (symbol, rhs) :: rules -> if guaranteed_termination rhs then symbol :: generate_white_list (start, rules) else generate_white_list (start, rules)
;;

(* returns list of all nonterminals which appear on lhs of grammar *)
let rec extract_all_lhs grammar =
match grammar with
| _, [] -> []
| start, (symbol, rhs) :: rules -> symbol :: extract_all_lhs (start, rules)
;;

(* returns list of all nonterminals which appear on rhs of grammar *)
let rec extract_all_rhs grammar =
match grammar with
| _, [] -> []
| start, (symbol, []) :: rules -> extract_all_rhs (start, rules)
| start, (symbol, N nonterminal :: rhs) :: rules -> nonterminal :: extract_all_rhs (start, (symbol, rhs) :: rules)
| start, (symbol, T _ :: rhs) :: rules -> extract_all_rhs (start, (symbol, rhs) :: rules)
;;

(* returns list of all nonterminals which where used on rhs but undefined on lhs of grammar *)
let generate_undefined_nonterminals grammar =
let all_lhs = extract_all_lhs grammar in
let all_rhs = extract_all_rhs grammar in
set_diff all_rhs all_lhs
;;

(* returns list of all nonterminals which where defined on lhs but never used on rhs of grammar *)
let generate_unused_nonterminals grammar =
let all_lhs = extract_all_lhs grammar in
let all_rhs = extract_all_rhs grammar in
set_diff all_lhs all_rhs
;;

(* returns list of all nonterminals which are guaranteed to never terminate *)
let generate_black_list grammar =
generate_unused_nonterminals grammar @ generate_undefined_nonterminals grammar
;;
