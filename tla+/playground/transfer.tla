---- MODULE transfer ----

(*****)
(*Useless transfer back and forth*)
(*****)
EXTENDS TLC, Integers

CONSTANT StartingBalance, TransferAmount

VARIABLES aBalance, bBalance, aTransfer, bTransfer

vars == << aBalance, bBalance, aTransfer, bTransfer >>

ASSUME /\ StartingBalance > 0
       /\ TransferAmount > 0
       /\ StartingBalance > TransferAmount

TypeOK ==
  /\ aBalance >= 0
  /\ bBalance >= 0
  /\ aTransfer \in Nat
  /\ bTransfer \in Nat
  /\ aBalance + bBalance + aTransfer + bTransfer = StartingBalance * 2

Init ==
  /\ aBalance = StartingBalance
  /\ bBalance = StartingBalance
  /\ bTransfer = 0
  /\ aTransfer = 0

ATransferToB ==
  /\ aBalance - TransferAmount > 0
  /\ aTransfer = 0
  /\ aBalance' = aBalance - TransferAmount
  /\ aTransfer' = TransferAmount
  /\ UNCHANGED << bTransfer, bBalance >>

BTransferToA ==
  /\ bBalance - TransferAmount > 0
  /\ bTransfer = 0
  /\ bBalance' = bBalance - TransferAmount
  /\ bTransfer' = TransferAmount
  /\ UNCHANGED << aTransfer, aBalance >>

AReceive ==
  /\ bTransfer /= 0
  /\ aBalance' = aBalance + bTransfer
  /\ bTransfer' = 0
  /\ UNCHANGED << bBalance, aTransfer >>

BReceive ==
  /\ aTransfer /= 0
  /\ bBalance' = bBalance + aTransfer
  /\ aTransfer' = 0
  /\ UNCHANGED << aBalance, bTransfer >>

Next == ATransferToB \/ BTransferToA \/ AReceive \/ BReceive

Spec == Init /\ [][Next]_vars

====