

---- MODULE overspending2 ----

(*************)
(* overspending1 works. This one will introduce TOCTOU bug between hasMoney and spend if they are two separate non-atomic steps *)
(* Race condition is added by adding to prepare spend in the withdraw step. This simulates in-flight async result while another withdraw request can come through *)
(* This seems more complex but it reflect perfectly what's happening in the real system. This is a TERRIBLE way of writing a spec, I think *)
(*************)
EXTENDS TLC, Integers, Naturals, Sequences

CONSTANTS Users

VARIABLES accounts, toSpend

vars == << accounts, toSpend >>

TypeOK ==
  /\ \A u \in Users: accounts[u] >= 0
  /\ toSpend \in [Users -> Seq(Nat)]
  /\ DOMAIN toSpend \subseteq Users
  /\ \A k \in DOMAIN toSpend: toSpend[k] \in Seq(Nat)

Init ==
  /\ accounts = [a \in Users |-> 0]
  /\ toSpend = [u \in Users |-> <<>>]

Deposit(u, amount) ==
  /\ accounts' = [accounts EXCEPT ![u] = accounts[u] + amount]
  /\ UNCHANGED << toSpend >>

Spend(u) ==
  /\ toSpend[u] # <<>>
  /\ toSpend' = [toSpend EXCEPT ![u] = Tail(toSpend[u])]
  /\ accounts' = [accounts EXCEPT ![u] = accounts[u] - Head(toSpend[u])]

PrepareWithdraw(u, amount) ==
  /\ accounts[u] - amount >= 0
  /\ toSpend' = [toSpend EXCEPT ![u] = toSpend[u] \o << amount >>]
  /\ UNCHANGED << accounts >>

Next ==
  \E amount \in 1 .. 10:
    \E u \in Users: Deposit(u, amount) \/ PrepareWithdraw(u, amount) \/ Spend(u)

Spec == Init /\ [][Next]_vars
====
