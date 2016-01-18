open List;;

(* Utility functions *)

(* returns true if input contains 'obj' (semantic), else false*)
let rec contains list obj =
match list with
| [] -> false
| h :: t -> if h = obj then true else contains t obj;;

(* returns a list with ALL elements semantically equal to obj removed*)
let rec remove obj = function
| [] -> []
| h :: t -> if h = obj then remove obj t else h :: remove obj t;;

(* Set functions *)

(* returns true if a is a proper subset of b *)
let rec proper_subset a b =
match a with
| [] -> if b != [] then true else false
| h :: t -> if contains b h then proper_subset (remove h t) (remove h b) else false;;

(* returns true if a is equal to b (using set logic, where all elements of set are unique) *)
let rec equal_sets a b =
match a with
| [] -> if b = [] then true else false
| h :: t -> if contains b h then equal_sets (remove h t) (remove h b) else false;;

(* returns true if a is a (strict) subset of b *)
let rec subset a b =
match a with
| [] -> true
| _ -> if proper_subset a b || equal_sets a b then true else false;;

(* returns a list of all elements in a which are not also in b (set difference) *)
let rec set_diff a b = match a with
| [] -> []
| h :: t -> if contains b h then set_diff (remove h t) (remove h b) else h :: set_diff (remove h t) (remove h b);;

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

(* returns list of all rules for nonterminal -- checked*)
let rec get_rules nonterminal grammar =
match grammar with
| _, [] -> []
| start, (symbol, rhs) :: rules -> if nonterminal = symbol then (symbol, rhs) :: get_rules nonterminal (start, rules) else get_rules nonterminal (start, rules);;

(* returns true if all elements in rhs are terminals -- checked*)
let rec guaranteed_termination rhs =
match rhs with
| [] -> true
| N _ :: t -> false
| T _ :: t -> guaranteed_termination t
;;

(* returns list of all nonterminals which are guaranteed to terminate -- checked*)
let rec generate_white_list grammar white_list =
match grammar with
| start, [] -> white_list
| start, (symbol, rhs) :: rules ->
if contains white_list symbol then generate_white_list (start, rules) white_list
else if guaranteed_termination rhs then generate_white_list (start, rules) (symbol :: white_list) else generate_white_list (start, rules) white_list
;;

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
(* | start, (symbol, h :: rhs) :: rules -> h :: extract_all_rhs (start, (symbol, rhs) :: rules) *)
| start, (symbol, N nonterminal :: rhs) :: rules -> nonterminal :: extract_all_rhs (start, (symbol, rhs) :: rules)
| start, (symbol, T _ :: rhs) :: rules -> extract_all_rhs (start, (symbol, rhs) :: rules)
;;

(* returns list of all nonterminals which where used on rhs but undefined on lhs of grammar *)
let generate_undefined_nonterminals grammar =
let all_lhs = extract_all_lhs grammar in
let all_rhs = extract_all_rhs grammar in
set_diff all_rhs all_lhs
;;

(* returns list of all nonterminals which were defined on lhs but never used on rhs of grammar -- broken!*)
(* let generate_unused_nonterminals grammar =
let all_lhs = filter (fun x -> if contains (generate_white_list grammar) x then False else True) (extract_all_lhs grammar) in
let all_rhs = filter (fun x -> if contains (generate_white_list grammar) x then False else True) (extract_all_rhs grammar) in
set_diff all_lhs all_rhs
;; *)

(* returns list of all nonterminals which are guaranteed to never terminate -- broken!*)
let generate_black_list grammar =
generate_undefined_nonterminals grammar
(* @ generate_unused_nonterminals grammar *)
;;

let rec extract_nonterminals rhs =
match rhs with
| [] -> []
| N nonterminal :: t -> nonterminal :: extract_nonterminals t
| T _ :: t -> extract_nonterminals t
;;

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

let rec aux_filter_blind_alleys grammar white_list =
match grammar with
| _, [] -> white_list
| start, (symbol, rhs) :: rules ->
if contains white_list symbol then aux_filter_blind_alleys (start, rules) white_list
else if subset (extract_nonterminals rhs) white_list then symbol :: white_list
else aux_filter_blind_alleys (start, rules) white_list
;;

let rec iter_filter_blind_alleys grammar white_list length =
match length with
| 0 -> white_list
| _ -> iter_filter_blind_alleys grammar (aux_filter_blind_alleys grammar white_list) (length - 1)
;;

let generate_valid_nonterminals grammar =
let length = length (unique (extract_all_lhs grammar)) in
let white_list = generate_white_list grammar [] in
iter_filter_blind_alleys grammar white_list length
;;

(* let rec filter_unterminatable grammar =
let valid_nonterminals = generate_valid_nonterminals grammar in
match grammar with
| _, [] -> []
| start, (symbol, rhs) :: rules ->
if contains valid_nonterminals symbol then (symbol, rhs) :: filter_unterminatable (start, rules) else filter_unterminatable (start, rules) *)


let rec filter_nonterminatable grammar white_list =
match grammar with
| _, [] -> []
| start, (symbol, rhs) :: rules -> if contains white_list symbol then (symbol, rhs) :: filter_nonterminatable (start, rules) white_list  else filter_nonterminatable (start, rules) white_list
;;

let rec filter_black_list grammar black_list =
match grammar with
| _, [] -> []
| start, (symbol, rhs) :: rules -> if contains black_list symbol || not (equal_sets (set_diff black_list (extract_nonterminals rhs)) black_list) then  filter_black_list (start, rules) black_list else (symbol, rhs) :: filter_black_list (start, rules) black_list
;;

let rec filter_blind_alleys grammar =
let white_list = generate_valid_nonterminals grammar in
let black_list = generate_black_list grammar in
match grammar with
| start, [] -> start, []
| start, (symbol, rhs) :: rules -> start, filter_black_list (start, (filter_nonterminatable grammar white_list)) black_list
;;
