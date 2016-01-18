(* Utility functions *)

(* returns a list with ALL elements semantically equal to obj removed*)
let rec remove obj = function
| [] -> []
| h :: t -> if h = obj then remove obj t else h :: remove obj t;;

(* returns true if input contains 'obj' (semantic), else false*)
let rec contains list obj =
match list with
| [] -> false
| h :: t -> if h = obj then true else contains t obj;;

let rec unique list =
match list with
| [] -> []
| h :: t -> if contains t h then unique t else h :: unique t
;;

let rec length list =
match list with
| [] -> 0
| h :: t -> 1 + length t
;;

(* Set functions *)

(* returns true if a is a proper subset of b *)
let rec proper_subset a b =
match a with
| [] -> if b != [] then true else false
| h :: t -> if contains b h then proper_subset (remove h t) (remove h b) else 
false;;

(* returns true if a is equal to b (using set logic, where all elements of set 
are unique) *)
let rec equal_sets a b =
match a with
| [] -> if b = [] then true else false
| h :: t -> if contains b h then equal_sets (remove h t) (remove h b) else 
false;;

(* returns true if a is a (strict) subset of b *)
let rec subset a b =
match a with
| [] -> true
| _ -> if proper_subset a b || equal_sets a b then true else false;;

(* returns a list of all elements in a which are not also in b (set difference) 
*)
let rec set_diff a b = match a with
| [] -> []
| h :: t -> if contains b h then set_diff (remove h t) (remove h b) else h :: 
set_diff (remove h t) (remove h b);;

(* returns a list of the union of a and b *)
let set_union a b =
unique (a @ b)
;;

(* returns a list containing the intersection of a and b *)
let set_intersection a b =
set_diff (set_union a b) ((set_diff a b) @ (set_diff b a))
;;

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

(* Blind alley removal functions*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Blind alley removal utitlities*)

(* returns list of all nonterminals which appear on lhs of grammar -- checked*)
let rec extract_all_lhs grammar =
match grammar with
| _, [] -> []
| start, (symbol, []) :: rules -> extract_all_lhs (start, rules)
| start, (symbol, rhs) :: rules -> symbol :: extract_all_lhs (start, rules)
;;

(* returns list of all nonterminals which appear on rhs of grammar -- checked*)
let rec extract_all_rhs grammar =
match grammar with
| _, [] -> []
| start, (symbol, []) :: rules -> extract_all_rhs (start, rules)
| start, (symbol, N nonterminal :: rhs) :: rules -> nonterminal :: 
extract_all_rhs (start, (symbol, rhs) :: rules)
| start, (symbol, T _ :: rhs) :: rules -> extract_all_rhs (start, (symbol, rhs) 
:: rules)
;;

let rec extract_nonterminals rhs =
match rhs with
| [] -> []
| N nonterminal :: t -> nonterminal :: extract_nonterminals t
| T _ :: t -> extract_nonterminals t
;;

(* returns true if all elements in rhs are terminals -- checked*)
let rec guaranteed_termination rhs =
match rhs with
| [] -> true
| N _ :: t -> false
| T _ :: t -> guaranteed_termination t
;;

(* white list generation *)

(* returns list of all nonterminals which are guaranteed to terminate -- 
checked *)
let rec generate_initial_white_list grammar white_list =
match grammar with
| start, [] -> white_list
| start, (symbol, rhs) :: rules ->
if contains white_list symbol then generate_initial_white_list (start, rules) 
white_list
else if guaranteed_termination rhs then generate_initial_white_list (start, 
rules) (symbol :: white_list)
else generate_initial_white_list (start, rules) white_list
;;

let rec aux_generate_white_list grammar white_list =
match grammar with
| _, [] -> white_list
| start, (symbol, rhs) :: rules ->
if contains white_list symbol then aux_generate_white_list (start, rules) 
white_list
else if subset (extract_nonterminals rhs) white_list then symbol :: white_list
else aux_generate_white_list (start, rules) white_list
;;

let rec iter_generate_white_list grammar white_list count =
match count with
| 0 -> white_list
| _ -> iter_generate_white_list grammar (aux_generate_white_list grammar 
white_list) (count - 1)
;;

let generate_white_list grammar =
let count = length (unique (extract_all_lhs grammar)) in
let initial_white_list = generate_initial_white_list grammar [] in
iter_generate_white_list grammar initial_white_list count
;;

(* filter blind alleys *)

let rec rule_contains_only_white_listed_nonterminals white_list rule =
match rule with
| symbol, [] -> if contains white_list symbol then true else false
| symbol, N h :: t -> if contains white_list h then 
rule_contains_only_white_listed_nonterminals white_list (symbol, t) else false
| symbol, T _ :: t -> rule_contains_only_white_listed_nonterminals white_list 
(symbol, t)

let rec aux_filter_blind_alleys white_list rules =
match rules with
| [] -> []
| h :: t -> if rule_contains_only_white_listed_nonterminals white_list h then h 
:: aux_filter_blind_alleys white_list t
else aux_filter_blind_alleys white_list t
;;

let filter_blind_alleys grammar =
let white_list = generate_white_list grammar in
match grammar with
| _, [] -> grammar
| start, rules -> start, (aux_filter_blind_alleys white_list rules)
