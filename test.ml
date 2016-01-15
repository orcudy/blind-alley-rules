
type nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num | DummyA | DummyB | DummyC

let rules =
   [
    DummyC, [N Expr; T ""; N Num];

    Expr, [T"("; N Expr; T")"];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N DummyA; T ""];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Expr, [N Num];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    DummyB, [N Expr; T ""; N Num];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let grammar = Expr, rules
