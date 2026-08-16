---- MODULE overspending ----

(* Basic overspending. This is correct. Withdraw and check are one atomic step *)
(* This can run forever without breaking invariants *)
EXTENDS TLC, Integers, Naturals

CONSTANTS Users

VARIABLES accounts

TypeOK ==
  /\ \A u \in Users: accounts[u] >= 0

Init ==
  /\ accounts = [a \in Users |-> 0]

Deposit(u, amount) ==
  /\ accounts' = [accounts EXCEPT ![u] = accounts[u] + amount]

Withdraw(u, amount) ==
  /\ accounts[u] - amount >= 0
  /\ accounts' = [accounts EXCEPT ![u] = accounts[u] - amount]

Next ==
  \E amount \in 1 .. 2:
    \E u \in Users: Deposit(u, amount) \/ Withdraw(u, amount)

Spec == Init /\ [][Next]_<< accounts >>
====
