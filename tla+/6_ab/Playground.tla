---- MODULE Playground ----
EXTENDS TLC, Integers, Sequences

Remove(i, seq) ==
  [j \in 1 .. ( Len(seq) - 1 ) |-> IF j < i THEN seq[j] ELSE seq[j + 1]
  ]

\* Use assume to test expressions
ASSUME PrintT(Remove(2, << "a" >>))
ASSUME PrintT(( ( 1 .. 3 ) \X { "a", "b" } ))

====