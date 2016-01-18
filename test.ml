
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
    Num, [T"9"]];;
  let grammar = Expr, rules;;


  let awksub_rules =
  [ Expr, [T"("; N Expr; T")"];
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

  type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

  let giant_grammar =
  Conversation,
  [ Snore, [T"ZZZ"];
    Quiet, [];
    Grunt, [T"khrgh"];
    Shout, [T"aooogah!"];
    Sentence, [N Quiet];
    Sentence, [N Grunt];
    Sentence, [N Shout];
    Conversation, [N Snore];
    Conversation, [N Sentence; T","; N Conversation]];;
