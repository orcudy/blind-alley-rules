open Utils
open Points

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
