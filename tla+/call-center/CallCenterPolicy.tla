---- MODULE CallCenterPolicy ----
EXTENDS TLC, Integers, FiniteSets

(****************************************************************************)
\* This algorithm model backend decision between four parties 
\* client, backend, voip, provider.
\*
\* (there actually is a third party, "customer", the one we call and call us, but that is directly
\* mediated by `provider`(twilio) for all flows so it can be simplified out)
\*
\* -> agents is our call-center agents(not the ai agents, the human agents)
\* -> backend is the backend handling backend as a part of its business logic
\* -> voip is the service `provider` calls when a call comes in or goes out. This also issue call token. The issued token can expire.
\* -> provider is the third party service that forward calls between backend and client.
\* -> decider is a different backend service that decides wher to route the customer's call to
\* In any state of the system, let the constant `MaxCon` be maximum configured concurrency of the system and
\* `Availability` be connected agents, the variable `con` representing ongoing call must never exceed MaxCon
\* 
\* All possible communications
\* The communication flow is:
\*                  - getting a token: (client -> backend -> voip) [bump availability]
\*                  - start outgoing: (client -> (provider -> voip -> backend)) [increases concurrency]
\*                  - start incoming: (provider -> (voip -> backend)) -> client[rings] [increases concurrency]
\*                  - getting a token: client -> backend
\*                  - termination: (client <-> provider) -> voip[tracks status]
\* parentheses implies request and response
\* For this first version of the spec, we model only the customer calling our service to keep things simple.
\* In fact this should already cover the ALL race conditions since check happen at the same place, but
\* for robustness, bi-directional call will also be done in a separate (refined?) spec.
(****************************************************************************)
CONSTANTS AGENTS, CUSTOMERS

CONSTANTS MAX_CONCURRENCY

VARIABLES agentStates,
          customerStates,
          \* Represent calls a customer make
          incomingCall,
          \* Represent customer calls that are matched to an agent
          matchedCall,
          concurrency 
actorVars == << agentStates, customerStates >>
concurrencyVars == <<concurrency>>
callVars == << incomingCall, matchedCall >>
vars == << actorVars, callVars, concurrencyVars >>

agentsOnlineAndAvailable == Cardinality({a \in AGENTS: agentStates[a] = "connected"})

concurrencyOK ==
  /\ agentsOnlineAndAvailable > concurrency
  /\ MAX_CONCURRENCY > concurrency

TypeOK ==
  /\ agentStates \in
       [AGENTS -> { "connected", "notConnected", "calling", "busy" }]
  /\ customerStates \in [CUSTOMERS -> { "calling", "idle", "busy" }]
  /\ incomingCall \in CUSTOMERS
  \* ringing is customer calling, rejected/accepted is agent reacting, and acknowledged is the customer's side has ackowledged
  \* it has happen and will either become busy / idle again based on whether they were rejected or accepted.
  /\ matchedCall \in [from : CUSTOMERS, to : AGENTS, status : {"ringing", "rejected", "accepted", "acknowledged" }]
  /\ concurrency >= 0 /\ concurrency \in Int
  /\ ( AGENTS \cap CUSTOMERS ) = {}

ConcurrencyOK == concurrencyOK

Init ==
  /\ agentStates = [a \in AGENTS |-> "notConnected"]
  /\ concurrency = 0
  /\ matchedCall = {}
  /\ incomingCall = {}
  /\ customerStates = [c \in CUSTOMERS |-> "idle"]

CustomerCalls ==
  /\ \E c \in CUSTOMERS:
       IF customerStates[c] = "idle"
       THEN /\ customerStates' = [customerStates EXCEPT ![c] = "calling"]
            /\ incomingCall' = incomingCall \cup {c}
       ELSE UNCHANGED << customerStates, incomingCall >>
  /\ UNCHANGED << agentStates, concurrencyVars, matchedCall >>

ForwardCallToAgent ==
  /\ \E call \in incomingCall : 
     /\ incomingCall' = incomingCall \ {call}
     /\ IF concurrencyOK 
        THEN /\ concurrency' = concurrency + 1
             /\ LET 
                  picked == CHOOSE a \in AGENTS : 
                    \* This means we only ring agent who is not in a call with anyone.
                    /\ agentStates[a] = "connected"
                  newState == [from: call, to: picked, status: "ringing"]
                IN
                  /\ Assert(newState \cap matchedCall = {}, "new state overlaps")
                  /\ matchedCall' = (matchedCall \cup newState)
        ELSE UNCHANGED << matchedCall, concurrencyVars >>
    /\ UNCHANGED << actorVars >>

\* here agent picks up
AgentPicksUpOrRejects ==
  /\ \E m \in matchedCall :
      /\ m.status = "ringing"
      /\ Assert(agentStates[m.to] = "connected", "Call should be forwarded to agents that are not busy.")
      /\ agentStates' = [agentStates EXCEPT![m.to] = "busy"]
      /\ \/ matchedCall' = (matchedCall \ {m}) \cup {[m EXCEPT !.status = "accepted"]}
         \* Not testing timeout because timeout and rejected are the same thing
         \/ matchedCall' = (matchedCall \ {m}) \cup {[m EXCEPT !.status = "rejected"]}
      /\ UNCHANGED << customerStates, concurrencyVars, incomingCall >>

\* here customer becomes busy(agent accept) or remains in the same state (client rejects). The time between agent picks up and customer picks up is almost immeidate.
CustomerAcknowledgeClient ==
    /\ \E m \in matchedCall :
        /\ m.status \in {"accepted", "rejected"}
        /\ Assert(customerStates[m.from] # "busy", "Client state can't be busy here")
        /\ customerStates' = [customerStates EXCEPT![m.from] = "busy"]
        /\ matchedCall' = (matchedCall \ {m}) \cup {[m EXCEPT !.status = "acknowledged"]}
        /\ UNCHANGED << agentStates, concurrencyVars, incomingCall >>

AgentComesOnline ==
  /\ \E a \in AGENTS :
    /\ agentStates[a] = "notConnected"
    /\ agentStates = [agentStates EXCEPT![a] = "connected"]
    /\ UNCHANGED << customerStates, concurrencyVars, callVars >>

AgentGoesOffline == 
  /\ \E a \in AGENTS :
    \* This check is enough because agent can goes offline at any time. Even while customer is ringing.
    /\ agentStates[a] = "connected"
    /\ agentStates' = [agentStates EXCEPT![a] = "notConnected"]
    /\ UNCHANGED << customerStates, concurrencyVars, callVars >>

\* Call drop, agent hangs up, customer hangs up, whatever.
CallEnds == 
  /\ \E m \in matchedCall :
    \* Any matched call can be dropped
    /\ matchedCall' = (matchedCall \ {m})
    /\ agentStates' = [agentStates EXCEPT![m.to] = "connected"]
    /\ customerStates' = [customerStates EXCEPT![m.from] = "idle"]
    /\ UNCHANGED << concurrencyVars, incomingCall >>

Next == 
  \/ CustomerCalls 
  \/ ForwardCallToAgent
  \/ AgentPicksUpOrRejects
  \/ CustomerAcknowledgeClient
  \/ AgentComesOnline
  \/ AgentGoesOffline
  \/ CallEnds

Spec == Init /\ [][Next]_vars

\* Token can only expire if agent is not online because twilio constantly refreshes the token for us.
====
