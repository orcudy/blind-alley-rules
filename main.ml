open Utils
open Points
open Rules

let test actual expected =
if actual = expected then (Printf.printf "Passed\n") else (Printf.printf"Failed\n");;

(* affirmative tests *)
test (Utils.subset [1;2;3] [1;2;3;4;5]) true;;
test (Utils.equal_sets [1;2;3;4;5] [1;1;2;2;3;4;5;5]) true;;
test (Utils.equal_sets (Utils.set_union [1;2;3] [4;5]) [1;2;3;4;5]) true;;
test (Utils.equal_sets (Utils.set_intersection [1;2;3;4;5] [1;2;3;6;7]) [1;2;3]) true;;
test (Utils.equal_sets (Utils.set_diff [1;2;3;4;5] [1;1;2;3]) [4;5]) true;;

test (Points.computed_fixed_point (=) (fun x -> x) 1 = 1) true;;
test (Points.computed_periodic_point (=) (fun x -> x) 1 1 = 1) true;;

(* negative tests *)
test (Utils.subset [1;2;3] [1;3;5]) false;;
test (Utils.equal_sets [1;2;3;4] [1;1;2;2;3;4;5;5]) false;;
test (Utils.equal_sets (Utils.set_union [1;2;3] [4;5]) [1;4;5]) false;;
test (Utils.equal_sets (Utils.set_intersection [1;2;3;4;5] [5]) [1;2;3]) false;;
test (Utils.equal_sets (Utils.set_diff [1;2;3;4;5] [1;1;2;3]) [1;2]) false;;
										      
(* filter blind alley test *)

type awksub_nonterminals = Expr | Lvalue | Incrop | Binop | Num;;

let awksub_rules =
[Expr, [T"("; N Expr; T")"];
Expr, [N Num];
Expr, [N Expr; N Binop; N Expr];
Expr, [N Lvalue];
Expr, [N Incrop; N Lvalue];
Expr, [N Lvalue; N Incrop];
Lvalue, [T"$"; N Expr];
Incrop, [T"++"];
Incrop, [T"--"];
Binop, [T"+"];
Binop, [T"-"];
Num, [T"0"];
Num, [T"1"];
Num, [T"2"];
Num, [T"3"];
Num, [T"4"];
Num, [T"5"];
Num, [T"6"];
Num, [T"7"];
Num, [T"8"];
Num, [T"9"]];;

let awksub_grammar = Expr, awksub_rules;;

test (filter_blind_alleys awksub_grammar) awksub_grammar;;
test (filter_blind_alleys (Expr, List.tl awksub_rules)) (Expr, List.tl awksub_rules);;

test
  (filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]]))
   (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]]);;



