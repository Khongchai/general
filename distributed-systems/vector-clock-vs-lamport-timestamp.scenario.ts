import {
  LamportTimestamp,
  VectorClock,
} from "./vector-clock-vs-lamport-timestamp";

// Node indices / IDs used throughout: A = 0, B = 1, C = 2.
const A = 0,
  B = 1,
  C = 2;

const lt = (t: LamportTimestamp) => `(${t.clock}, node ${t.nodeId})`;
const vc = (v: VectorClock) => `[${v.join(", ")}]`;

function report(
  title: string,
  x: { name: string; lt: LamportTimestamp; vc: VectorClock },
  y: { name: string; lt: LamportTimestamp; vc: VectorClock },
) {
  console.info(`\n${title}`);
  console.info(
    `  Lamport: ${x.name} ${lt(x.lt)} vs ${y.name} ${lt(y.lt)} -> ${x.lt.compare(y.lt)}`,
  );
  console.info(
    `  Vector:  ${x.name} ${vc(x.vc)} vs ${y.name} ${vc(y.vc)} -> ${x.vc.compare(y.vc)}`,
  );
}

// ---------------------------------------------------------------------------
// Scenario 1: concurrent, but with DIFFERENT counters.
// A does two local events, B does one. They never talk.
// Lamport: "after" (looks causal, isn't). Vector: "concurrent".
// ---------------------------------------------------------------------------
{
  const aLt = new LamportTimestamp(0, A).increment().increment();
  const bLt = new LamportTimestamp(0, B).increment();
  const aVc = new VectorClock(0, 0).increment(A).increment(A);
  const bVc = new VectorClock(0, 0).increment(B);

  report(
    "1. Concurrent events with different counters",
    { name: "A's 2nd event", lt: aLt, vc: aVc },
    { name: "B's 1st event", lt: bLt, vc: bVc },
  );
}

// ---------------------------------------------------------------------------
// Scenario 2: a causal chain A -> B -> C. Both agree.
// A sends to B, B receives and forwards to C, C receives.
// A and C never talk directly, but A's send still happens before C's receive.
// Lamport: "before". Vector: "before". (Lamport's one guarantee holds.)
// ---------------------------------------------------------------------------
{
  // A sends
  const aSendLt = new LamportTimestamp(0, A).increment();
  const aSendVc = new VectorClock(0, 0, 0).increment(A);

  // B receives, then sends to C
  const bRecvLt = new LamportTimestamp(0, B).receive(aSendLt);
  const bRecvVc = new VectorClock(0, 0, 0).receive(aSendVc, B);
  const bSendLt = bRecvLt.increment();
  const bSendVc = bRecvVc.increment(B);

  // C receives
  const cRecvLt = new LamportTimestamp(0, C).receive(bSendLt);
  const cRecvVc = new VectorClock(0, 0, 0).receive(bSendVc, C);

  report(
    "2. Causal chain A -> B -> C",
    { name: "A's send", lt: aSendLt, vc: aSendVc },
    { name: "C's receive", lt: cRecvLt, vc: cRecvVc },
  );
}

// ---------------------------------------------------------------------------
// Scenario 3: a busy bystander.
// A sends to B, B receives. Meanwhile C does 10 local events and talks to nobody.
// Lamport: B's receive is "before" C's 10th event, just because C is busy.
// Vector: "concurrent".
// ---------------------------------------------------------------------------
{
  const aSendLt = new LamportTimestamp(0, A).increment();
  const aSendVc = new VectorClock(0, 0, 0).increment(A);
  const bRecvLt = new LamportTimestamp(0, B).receive(aSendLt);
  const bRecvVc = new VectorClock(0, 0, 0).receive(aSendVc, B);

  let cLt = new LamportTimestamp(0, C);
  let cVc = new VectorClock(0, 0, 0);
  for (let i = 0; i < 10; i++) {
    cLt = cLt.increment();
    cVc = cVc.increment(C);
  }

  report(
    "3. Busy bystander",
    { name: "B's receive", lt: bRecvLt, vc: bRecvVc },
    { name: "C's 10th event", lt: cLt, vc: cVc },
  );
}

// U UNDERSTAND THIS U UNDERSTAND LAMPORT TIMESTAMP AND VECTOR CLOCK
// ---------------------------------------------------------------------------
// Scenario 4: the two cases Lamport can't tell apart (offline sync).
// Setup: A makes an edit; B syncs it (merge only, no increment: B now simply
// holds A's state, it hasn't done anything of its own). Then A goes on to make
// two more edits, and we compare A's latest version with B's latest version.
//   (a) Stale copy:    B does nothing more.        -> safe to overwrite B.
//   (b) Real conflict: B makes its own edit offline -> must keep both.
// Lamport says "after" in BOTH cases, so last-writer-wins drops B's edit in (b).
// Vector says "after" in (a) and "concurrent" in (b).
// ---------------------------------------------------------------------------
{
  // Shared starting point
  const aEdit1Lt = new LamportTimestamp(0, A).increment();
  const aEdit1Vc = new VectorClock(0, 0).increment(A);
  const bSyncedLt = new LamportTimestamp(0, B).merge(aEdit1Lt);
  const bSyncedVc = new VectorClock(0, 0).merge(aEdit1Vc);

  // A keeps editing (in both cases)
  const aLatestLt = aEdit1Lt.increment().increment();
  const aLatestVc = aEdit1Vc.increment(A).increment(A);

  // (a) B does nothing more
  report(
    "4a. B's copy is just stale",
    { name: "A's latest", lt: aLatestLt, vc: aLatestVc },
    { name: "B's latest", lt: bSyncedLt, vc: bSyncedVc },
  );

  // (b) B edits offline, without seeing A's later edits
  const bEditLt = bSyncedLt.increment();
  const bEditVc = bSyncedVc.increment(B);
  report(
    "4b. B edited independently",
    { name: "A's latest", lt: aLatestLt, vc: aLatestVc },
    { name: "B's latest", lt: bEditLt, vc: bEditVc },
  );
}

// ---------------------------------------------------------------------------
// Scenario 5: compare AFTER merge erases the evidence.
// A and B each do one local event (concurrent). Compare before merging, then
// merge B into A and compare again. After the merge it can never be "concurrent".
// (Vector clocks only; this is about ordering compare vs merge.)
// ---------------------------------------------------------------------------
{
  const aVc = new VectorClock(0, 0).increment(A);
  const bVc = new VectorClock(0, 0).increment(B);
  const merged = aVc.merge(bVc);

  console.info("\n5. Compare before vs after merge");
  console.info(
    `  Before merge: A ${vc(aVc)} vs B ${vc(bVc)} -> ${aVc.compare(bVc)}`,
  );
  console.info(
    `  After merge:  A ${vc(merged)} vs B ${vc(bVc)} -> ${merged.compare(bVc)}`,
  );
}

// ---------------------------------------------------------------------------
// Scenario 6: catch-up guarantee.
// A does 100 events; its 100th is a send to B. B had done 1 local event,
// receives A's message, then makes an edit. B's edit beats A's send, even
// though B was far behind: anything that has SEEN a write comes after it.
// Lamport: "after". Vector: "after".
// ---------------------------------------------------------------------------
{
  let aLt = new LamportTimestamp(0, A);
  let aVc = new VectorClock(0, 0);
  for (let i = 0; i < 100; i++) {
    aLt = aLt.increment();
    aVc = aVc.increment(A);
  }
  // aLt / aVc is now A's 100th event: the send

  const bLocalLt = new LamportTimestamp(0, B).increment();
  const bLocalVc = new VectorClock(0, 0).increment(B);
  const bRecvLt = bLocalLt.receive(aLt);
  const bRecvVc = bLocalVc.receive(aVc, B);
  const bEditLt = bRecvLt.increment();
  const bEditVc = bRecvVc.increment(B);

  report(
    "6. Catch-up after receiving",
    { name: "B's edit", lt: bEditLt, vc: bEditVc },
    { name: "A's send", lt: aLt, vc: aVc },
  );
}
