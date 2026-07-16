---- MODULE SimpleServerClient ----
EXTENDS TLC

VARIABLES serverState, clientState, msgs
\* All messages that has ever been seen.
\* This syntax works too and more concise
\* Messages == [type: {"Hello"}, sender, {"client"}] \cup [type: {"Oh, hi!"}, sender, {"server"}]
Messages ==
  { [ type |-> "Hello!", sender |-> "client" ],
    [ type |-> "Oh, hi!", sender |-> "server" ]
  }

TypeOK ==
  /\ msgs \subseteq Messages
  /\ serverState \in { "idle", "sending", "receiving" }
  /\ clientState \in { "idle", "sending" }

Init ==
  /\ serverState = "idle"
  /\ clientState = "idle"
  /\ msgs = {}

ClientSending ==
  /\ serverState = "idle"
  /\ clientState = "idle"
  /\ clientState' = "sending"
  /\ msgs = {}
  /\ msgs' = { [ type |-> "Hello!", sender |-> "client" ] }
  /\ UNCHANGED << serverState >>

ServerReceiving ==
  /\ serverState = "idle"
  /\ serverState' = "receiving"
  /\ clientState = "sending"
  /\ clientState' = "idle"
  /\ msgs = { [ type |-> "Hello!", sender |-> "client" ] }
  /\ msgs' = msgs \cup { [ type |-> "Oh, hi!", sender |-> "server" ] }

AllIdle ==
  /\ clientState = "idle"
  /\ clientState' = "idle"
  /\ serverState = "receiving"
  /\ serverState' = "idle"
  /\ msgs = Messages
  /\ msgs' = {}

Next == ClientSending \/ ServerReceiving \/ AllIdle
====