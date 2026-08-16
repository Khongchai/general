---- MODULE overspending ----
EXTENDS TLC, Integers, Naturals

CONSTANTS Users

VARIABLES accounts

TypeOK ==
  /\ \A u \in Users: accounts[u] >= 0

Init ==
  /\ accounts = [a \in Users |-> 0]

Deposit(u, amount) ==
  /\ accounts' = [accounts EXCEPT ![u] = amount]

Withdraw(u, amount) ==
  /\ accounts[u] - amount >= 0
  /\ accounts' = [accounts EXCEPT ![u] = accounts[u] - amount]

Next ==
  \E amount \in 1 .. 10:
    \E u \in Users: Deposit(u, amount) \/ Withdraw(u, amount)

Spec == Init /\ [][Next]_<< accounts >>

\* if (hasMoney(account, amount)) {
\*     spend(amount);
\* }
====
