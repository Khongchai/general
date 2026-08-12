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


\* Use assume to test expressions
\* ASSUME PrintT(Remove(2, << "a" >>))
\* ASSUME PrintT(( ( 1 .. 3 ) \X { "a", "b" } ))
\* ASSUME PrintT(Seq(( 1 .. 3 ) \X { "a", "b" }))
\* ASSUME PrintT(Seq({ 0, 1 }))
\* ASSUME PrintT([1 .. 4 -> { 0, 1 }])
\* ASSUME PrintT([1 .. 2 -> ( { "a", "b" } \X { 0, 1 } )])
ASSUME PrintT(RemoveX(<< "Tom", "X", "John", "Doe", "X" >>))
====