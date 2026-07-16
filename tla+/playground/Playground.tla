---- MODULE Playground ----
EXTENDS TLC, Integers

CONSTANT RM
VARIABLE rmState

Messages ==
  /\ PrintT(<< "Value", [RM -> { "prepared" }] >>)
  /\ PrintT(<< "Value",
          [type:{ "Prepared" }, rm:RM ] \cup [type:{ "Commit", "Abort" } ]
       >>)
TCTypeOK ==
  /\ rmState \in [RM -> { "working", "prepared", "committed", "aborted" }]

TCInit ==
  /\ rmState = [r \in RM |-> "working"]
  /\ Messages

TCNext == rmState = [r \in RM |-> "aborted"]

====