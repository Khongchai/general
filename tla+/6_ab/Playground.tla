---- MODULE Playground ----
EXTENDS TLC, Integers, Sequences

Remove(i, seq) ==
  [j \in 1 .. ( Len(seq) - 1 ) |-> IF j < i THEN seq[j] ELSE seq[j + 1]
  ]

RECURSIVE RemoveX(_)
RemoveX(seq) ==
  IF seq = <<>>
  THEN <<>>
  ELSE IF seq[1] = "X"
    THEN RemoveX(Tail(seq))
    ELSE << Head(seq) >> \o RemoveX(Tail(seq))

\* Need to build a set first 
Contains(seq, item) == item \in {seq[i]: i \in 1 .. Len(seq)}

RECURSIVE Omit(_,_)
Omit(seq, arr) ==
  IF seq = <<>>
  THEN <<>>
  ELSE IF Contains(arr, Head(seq))
    THEN Omit(Tail(seq), arr)
    ELSE << Head(seq) >> \o Omit(Tail(seq), arr)



\* Use assume to test expressions
\* ASSUME PrintT(Remove(2, << "a" >>))
\* ASSUME PrintT(( ( 1 .. 3 ) \X { "a", "b" } ))
\* ASSUME PrintT(Seq(( 1 .. 3 ) \X { "a", "b" }))
\* ASSUME PrintT(Seq({ 0, 1 }))
\* ASSUME PrintT([1 .. 4 -> { 0, 1 }])
\* ASSUME PrintT([1 .. 2 -> ( { "a", "b" } \X { 0, 1 } )])
\* ASSUME PrintT(RemoveX(<< "Tom", "X", "John", "Doe", "X" >>))
\* ASSUME PrintT(( { "d1", "d2" } \X { 0, 1 } ) \cup { << "0", "1" >> })
\* ASSUME PrintT([ a |-> 0, b |-> 1 ] \in [{ "a", "b" } -> Nat])
\* Next line asserts that the left and right hand side expressions' domains match exactly.
\* ASSUME PrintT([ u1 |-> << 1, 2, 3 >>, u2 |-> << 1, 2 >> ] \in
\*            [{ "u1", "u2" } -> Seq(Nat)])
\* Next line asserts that the left is a subset of right hand side
\* ASSUME PrintT(LET f1 == [ u1 |-> << 1, 2, 3 >> ]
\*                   members == { "u1", "u2" }
\*          IN /\ DOMAIN f1 \subseteq members
\*             /\ \A k \in DOMAIN f1: f1[k] \in Seq(Nat))
ASSUME PrintT(Omit(<< 1, 2, 3 >>, << 1, 3 >>))
====