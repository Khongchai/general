---- MODULE CallCenterPolicy ----
EXTENDS TLC, Integers, FiniteSets


\* TODO:  simplify first version should just be customers trying to call agent first, then
\* we     can expand to agent trying to call them. Simpler state management that way
\* TODO: try commenting out agentsOnline, it should fail
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

VARIABLES validTokens, agentStates, concurrency, customerStates, inflightCallsRequest, inflightCallResponse
actorVars == <<agentStates, customerStates, validTokens>>
concurrencyVars == <<concurrency>>
inflightVars == << inflightCallsRequest, inflightCallResponse >>
vars == << actorVars, inflightVars, concurrencyVars >>

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
  /\ inflightCalls \in ( AGENTS \cup CUSTOMERS )
  /\ concurrency >= 0 /\ concurrency \in Int
  /\ availability >= 0 /\ availability \in Int
  \* Agents and customers can't share common values since we keep track of inflightCalls by their names.
  /\ ( AGENTS \cap CUSTOMERS ) = {}

ConcurrencyOK == concurrencyOK

Init ==
  /\ validTokens = {}
  /\ agentStates = [a \in AGENTS |-> "notConnected"]
  /\ concurrency = 0
  /\ inflightCallsRequest = {}
  /\ inflightCallResponse = {}

\* CustomerRejects

CustomerCalls ==
  /\ \E c \in CUSTOMERS:
       IF customerStates[c] = "idle"
       THEN /\ customerStates' = [customerStates EXCEPT ![c] = "calling"]
            /\ inflightCallsRequest' = inflightCallsRequest \cup c
       ELSE UNCHANGED << customerStates, inflightCallsRequest >>
  /\ UNCHANGED << validTokens, agentStates, concurrency, inflightCallResponse >>

\* AgentCalls

\* AgentRejects

VOIPReceives ==
  /\ \E call \in inflightCallsRequest: 
     /\ inflightCallsRequest' = inflightCallsRequest \ {call}
     /\ IF concurrencyOK
        THEN /\ concurrency' = concurrency + 1
             /\ LET picked = IF call \in CUSTOMERS 
                         THEN CHOOSE a \in AGENTS : agentStates[a] = "connected"
                         ELSE CHOOSE a \in CUSTOMERS: customerStates[a] = "idle"
                IN inflightCallResponse' = inflightCallResponse \cup picked
             /\ UNCHANGED << actorVars >>
        ELSE UNCHANGED << actorVars, inflightCallResponse, concurrencyVars  >>

\* HandleIncoming ==
====
