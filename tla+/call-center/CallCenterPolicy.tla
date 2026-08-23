---- MODULE CallCenterPolicy ----
EXTENDS TLC, Integers, FiniteSets

\* TODO
\* Continue from agent picks up

\* we     can expand to agent trying to call them. Simpler state management that way
\* TODO: try commenting out agentsOnline, it should fail -- this should probably hint at availability being better inferrerd through real online state, not token.
\* TODO: liveness property check, eventually, same number of agents and customers are busy.
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

CONSTANTS maxConcurrency

VARIABLES validTokens, 
          agentStates, 
          customerStates, 
          \* Represent calls a customer make
          incomingCall,
          \* Represent customer calls that are matched to an agent
          matchedCall,
          concurrency 
actorVars == << agentStates, customerStates, validTokens >>
concurrencyVars == <<concurrency>>
callVars == << incomingCall, matchedCall >>
vars == << actorVars, callVars, concurrencyVars >>

availability == Cardinality(validTokens)

agentsOnline == Cardinality({a \in AGENTS: agentStates[a] = "connected"})

concurrencyOK ==
  /\ agentsOnline >= concurrency
  /\ availability >= concurrency
  /\ maxConcurrency >= concurrency

TypeOK ==
  /\ validTokens \in AGENTS
  /\ agentStates \in
       [AGENTS -> { "connected", "notConnected", "calling", "busy" }]
  /\ customerStates \in [CUSTOMERS -> { "calling", "idle", "busy" }]
  /\ incomingCall \in CUSTOMERS
  \* ringing is customer calling, rejected/accepted is agent reacting, and acknowledged is the customer's side has ackowledged
  \* it has happen and will either become busy / idle again based on whether they were rejected or accepted.
  /\ matchedCall \in [from : CUSTOMERS, to : AGENTS, status : {"ringing", "rejected", "accepted", "acknowledged"}]
  /\ concurrency >= 0 /\ concurrency \in Int
  /\ availability >= 0 /\ availability \in Int
  /\ ( AGENTS \cap CUSTOMERS ) = {}

ConcurrencyOK == concurrencyOK

Init ==
  /\ validTokens = {}
  /\ agentStates = [a \in AGENTS |-> "notConnected"]
  /\ concurrency = 0
  /\ matchedCall = {}
  /\ incomingCall = {}
  /\ customerStates = [c \in CUSTOMERS |-> "idle"]

CustomerCalls ==
  /\ \E c \in CUSTOMERS:
       IF customerStates[c] = "idle"
       THEN /\ customerStates' = [customerStates EXCEPT ![c] = "calling"]
            /\ incomingCall' = incomingCall \cup c
       ELSE UNCHANGED << customerStates, incomingCall >>
  /\ UNCHANGED << validTokens, agentStates, concurrencyVars, matchedCall >>

VOIPProcessIncomingCall ==
  /\ \E call \in incomingCall : 
     /\ incomingCall' = incomingCall \ {call}
     /\ IF concurrencyOK
        THEN /\ concurrency' = concurrency + 1
             /\ LET 
                  picked == CHOOSE a \in AGENTS : agentStates[a] = "connected"
                  newState == [from: call, to: picked, status: "ringing"]
                IN
                  /\ Assert(newState \cap matchedCall = {}, "new state overlaps")
                  /\ matchedCall' = (matchedCall \cup newState)
        ELSE UNCHANGED << matchedCall, concurrencyVars >>
    /\ UNCHANGED << actorVars >>

\* here agent picks up
AgentPicksUp ==
  /\ \E m \in matchedCall :
    /\ m.status = "ringing"
    /\ agentStates' = [agentStates EXCEPT![m.to] = "busy"]
    /\ agentstates

\* here customer becomes busy
\* VOIPLetCustomerKnowThatAgentPickedUp

\* ConnectCustomer

\* AgentRejects 

\* Rejection


\* HandleIncoming ==
====
