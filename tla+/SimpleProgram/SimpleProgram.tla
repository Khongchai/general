---- MODULE SimpleProgram ----
EXTENDS TLC, Integers

VARIABLES i, pc

Init == ( pc = "start" ) /\ ( i = 0 )

Next ==
  \/ /\ pc = "start"
     /\ i' \in 0 .. 1000
     /\ pc' = "middle"
  \/ /\ pc = "middle"
     /\ i' = i + 1
     /\ pc' = "done"

====