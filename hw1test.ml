let my_subset_test0 = subset [1;2;3] [1;2;3;4;5]
let my_equal_sets_test0 = equal_sets [1;2;3;4;5] [1;1;2;2;3;4;5;5]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [4;5]) [1;2;3;4;5]
let my_set_intersection_test0 =
  equal_sets (set_intersection [1;2;3;4;5] [1;2;3;6;7]) [1;2;3]
let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4;5] [1;1;2;3]) [4;5]

let my_computed_fixed_point_test0 =
    computed_fixed_point (=) (fun x -> x) 1 = 1
let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x) 1 1 = 1

type boring_nonterminals = A | B | C | D

  let boring_rules =
     [A, [N B];
      B, [N C];
      C, [T "c"];
      D, [N D]];

let boring_grammar = A, boring_rules

let my_boring_test0 =
  filter_blind_alleys boring_grammar =
    (A, [ A, [N B];
          B, [N C];
          C, [T "c"]])
