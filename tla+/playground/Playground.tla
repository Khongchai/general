---- MODULE Playground ----
EXTENDS TLC, Integers

CONSTANT RM
VARIABLE rmState

TCTypeOK ==
  /\ rmState \in [RM -> { "working", "prepared", "committed", "aborted" }]

PrintTester == TRUE /\ PrintT(<< "test print: ", [i \in 1 .. 5 |-> i * i] >>)

TCInit ==
  /\ rmState = [r \in RM |-> "working"]
  /\ PrintTester

TCNext == rmState = [r \in RM |-> "aborted"]

====