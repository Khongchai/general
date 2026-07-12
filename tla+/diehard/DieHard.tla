---- MODULE DieHard ----
EXTENDS Integers

VARIABLES small, big

sMax == 3
bMax == 5

\* This is for type-checking only
\* It's a precondition
TypeOK ==
  /\ small \in 0 .. sMax
  /\ big \in 0 .. bMax

Init ==
  /\ big = 0
  /\ small = 0

\* Next state formula describes all permitted steps:
\* Usually F_1 \/ F_2 \/ ... \/ F_n,
\* The jug-filling behavior has 3 kinds of steps: fill, empty, transfer
\* Without big' = big, this state would be true even when small is 3 and big is "abc". 
\* State transition must properly define the real, real-world transition that can happen under ANY condition.
\* Math is precise, programming is not. TLA+ is math!
FillSmall ==
  /\ small' = 3
  /\ big' = big

FillBig ==
  /\ small' = small
  /\ big' = 5

\* There are two cases:
\* There is room in big for all water in small
\* There isn't room in big for all water in small
SmallToBig ==
  LET available == ( bMax - big )
  IN IF big + small <= bMax
      THEN /\ big' = big + small
           /\ small' = 0
      ELSE /\ big' = big + ( available )
           /\ small' = small - ( available )

EmptyBig ==
  /\ big' = 0
  /\ small' = small

EmptySmall ==
  /\ big' = big
  /\ small' = 0

BigToSmall ==
  LET available == ( sMax - small )
  IN IF big + small <= sMax
      THEN /\ big' = 0
           /\ small' = big + small
      ELSE /\ small' = small + ( available )
           /\ big' = big - ( available )

Next ==
  \/ FillSmall
  \/ FillBig
  \/ EmptySmall
  \/ EmptyBig
  \/ SmallToBig
  \/ BigToSmall
====