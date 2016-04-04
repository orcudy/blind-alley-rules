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
