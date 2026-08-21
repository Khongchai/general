---- MODULE clock ----
EXTENDS TLC, Integers

VARIABLES hour

Init == hour = 12

Next == IF hour = 23 THEN hour' = 0 \* hour' is "next value of hour"
  ELSE hour' = hour + 1

Spec == Init /\ [][Next]_hour

\* This fails because clock is thrown in a lake...:p
\* https://www.hillelwayne.com/post/fairness/
Liveness == <>( hour = 15 )
====
