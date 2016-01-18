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

let rec proper_subset a b =
match a with
| [] -> if b != [] then true else false
| h :: t -> if contains h b then proper_subset (remove h t) (remove h b) else false;;

let rec equal_sets a b =
match a with
| [] -> if b = [] then true else false
| h :: t -> if contains h b then equal_sets (remove h t) (remove h b) else false;;

let rec subset a b =
match a with
| [] -> true
| _ -> if proper_subset a b || equal_sets a b then true else false;;

let rec set_diff a b = match a with
| [] -> []
| h :: t -> if contains h b then set_diff (remove h t) (remove h b) else h :: set_diff (remove h t) (remove h b);;

(* Fixed/periodic point functions *)

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

(* return true if input list contains terminal only*)
let rec contains_all_terminals = function
| [] -> true
| N _ :: t -> false
| T _ :: t -> true && contains_all_terminals t;;

(*returns list of all rules in grammar with one terminal on rhs*)
let rec extract_terminal_rules = function
| _, [] -> []
| start, (symbol, rhs) :: rules -> if contains_all_terminals rhs then (symbol, rhs) :: extract_terminal_rules (start, rules) else extract_terminal_rules (start, rules);;

(* returns list of all rules for nonterminal *)
let rec get_rules nonterminal grammar =
match grammar with
| _, [] -> []
| start, (symbol, rhs) :: rules -> if nonterminal = symbol then (symbol, rhs) :: get_rules nonterminal (start, rules) else get_rules nonterminal (start, rules);;

(*
returns true if all elements in rhs_derivation list (i.e. [T "("; N Expr; T ")"] )
are terminals or in the lhs_terminal_symbols list (i.e. [Incrop; Binop; Num] )
*)
let rec will_terminate lhs_terminal_symbols rhs_derivation =
match rhs_derivation with
| [] -> true
| T _ :: t -> will_terminate lhs_terminal_symbols t
| N h :: t -> subset [h] lhs_terminal_symbols && will_terminate lhs_terminal_symbols t;;

(*
returns true if at least one elements in expression_rules list (i.e. [(Expr, [T "("; N Expr; T ")"]); (Expr, [N Num])] ) has all rhs elements which can terminate.
*)
let rec at_least_one_terminates lhs_terminal_symbols expression_rules =
match expression_rules with
| [] -> false
| (expression, rhs) :: rules -> will_terminate lhs_terminal_symbols rhs || at_least_one_terminates lhs_terminal_symbols rules;;
