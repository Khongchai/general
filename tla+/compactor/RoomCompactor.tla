---- MODULE RoomCompactor ----
EXTENDS TLC, Naturals, Sequences, FiniteSets, Integers



\* This specification guarantees that no compaction will happen when rooms are not empty.
\* The configuration is N backends, M clients, and 1 flusher.
\* However, our spec need not be so complex. We are trying to test if the flush state can happen while room is empty or not.
\* not testing race between multiple rooms. Doing so would be testing an implementation unrelated to this spec and
\* all changes are already idempotent thanks to yjs
\* The spec below might seem simple, but it points to this fact:
\* 
\* // INVARIANT: no `await` between removeClient and rooms.delete.
\* For room to be "empty" (client state atomically marked offline since our model has only one)
\* clientState -> offline must be atomic.
\* room.removeClient(client);
\* const roomEmpty = room.clientCount === 0;
\* if (roomEmpty) rooms.delete(room.name);
\* 
\* And that when compacting message is going on, a client can join a room
\* we must make sure there is a locking mechanism (optimisitc or otherwise) to reject client state from becoming true while compaction is on going
\* In this spec, the compactor backs off by writing only if fenceValid is true
Contains(seq, item) == item \in {seq[i]: i \in 1 .. Len(seq)}

VARIABLES
  clientState,
  inflight,
  compactor,
  scatter,
  gather,    \* Abstract fencing token. Fence is invalid when client joins (fencing token bumped)
  fenceValid

\* scatter and gather are the inflight scatter-gather occupancy check result.
vars == << scatter, gather, clientState, inflight, compactor, fenceValid >>

Init ==
  /\ clientState = "offline"
  /\ inflight = FALSE
  /\ compactor = "idle"
  /\ scatter = FALSE
  /\ gather = FALSE
  /\ fenceValid = FALSE

TypeOK ==
  /\ clientState \in { "in-room", "offline" }
  /\ inflight \in BOOLEAN
  /\ compactor \in { "idle", "scatter", "gather", "compacting", "writing" }
  /\ scatter \in BOOLEAN
  /\ gather \in BOOLEAN
  /\ fenceValid \in BOOLEAN

NoCompactionWhileOccupied ==
  \* this might seem correct, but due to CAS, we have to restate it
    \* clientState = "in-room" => compactor # "writing"
    \* as "whenever the client is in the position to write, client must be offline (since client online immeidately means false fencing)"
    ( fenceValid /\ gather /\ compactor = "compacting"
    ) =>
    clientState = "offline"



ClientJoins ==
  /\ clientState = "offline"
  /\ fenceValid' = FALSE
  /\ clientState' = "in-room"
  /\ UNCHANGED << inflight, compactor, scatter, gather >>

ClientLeaves ==
  /\ clientState = "in-room"
  /\ clientState' = "offline"
  /\ UNCHANGED << inflight, compactor, scatter, gather, fenceValid >>

ClientSends ==
  /\ clientState = "in-room"
  /\ inflight' = TRUE
  /\ UNCHANGED << clientState, compactor, scatter, gather, fenceValid >>

\* compacter begin scattering by pulling in message from the inflight area (redis in our case) 
CompactorScatters ==
  /\ fenceValid' = TRUE
  /\ compactor = "idle"
  /\ compactor' = "scatter"
  /\ scatter' = ( clientState = "offline" )
  /\ UNCHANGED << clientState, inflight, gather >>

CompactorGathers ==
  /\ compactor = "scatter"
  /\ compactor' = "gather"
  /\ gather' = scatter
  /\ UNCHANGED << scatter, clientState, inflight, scatter, fenceValid >>

CompactorRuns ==
  /\ compactor = "gather"
  /\ compactor' = "compacting"
  /\ UNCHANGED << gather, scatter, clientState, inflight, fenceValid >>

\* Compare-and-swap as one single atomic step.
\* If we break CompactorWrite and CompactorDone into two different atomic step,
\* a client then can join while it's still on-going.
CompactorWriteAndDone ==
  /\ compactor = "compacting"
  /\ IF fenceValid /\ gather
     THEN /\ inflight' = FALSE
          /\ compactor' = "idle"
     ELSE /\ compactor' = "idle"
          /\ UNCHANGED inflight
  /\ UNCHANGED << gather, scatter, clientState, fenceValid >>

PrintAllStates == PrintT(vars)

Next ==
  \/ ClientJoins
  \/ ClientLeaves
  \/ ClientSends
  \/ CompactorScatters
  \/ CompactorGathers
  \/ CompactorRuns
  \/ CompactorWriteAndDone


\* Fairness and safety is an assumption about the world, remember the lake example. Remember this:
\* "fairness without a liveness property checks nothing". Fairness 
Fairness ==
  /\ WF_vars(CompactorScatters)
  /\ WF_vars(CompactorGathers)
  /\ WF_vars(CompactorRuns)
  /\ WF_vars(CompactorWriteAndDone)

Spec == Init /\ [][Next]_vars /\ Fairness

Liveness == <>[]( clientState = "offline" ) => <>( inflight = FALSE )

====