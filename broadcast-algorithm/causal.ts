/**
A note on how this Kleppmann's version relate to real vector clock. Real vector clock increments before sends
but his sends before increments. Kleppmann's seems to be a straight extension of his fifo example and according to Claude,
it's closer to real world implementation in broadcasts. But this is the same information just off by one.

The reason he only cares about <= instead of "concurrent", "equal", "before", "after" quadstate of vector clock is because
a node only has to know that it can send a message when all the messages dependencies are there. "when all the messages dependencies" is just `<=`.

 */

// global counter represents global id
let counter = 0;
type Message = string;

type BroadcastMessage = {
  message: Message;
  nodeId: number;

  // in fifo, it's sendSeq
  //   but for causal,
  deps: number[];
};

class NodeInstance {
  private _nodeId = counter++;
  private _delivered: number[] = [];
  private _buffer: BroadcastMessage[] = [];

  private _deliverToApplication: (m: string) => void;
  public constructor(deliverToApplication: typeof this._deliverToApplication) {
    this._deliverToApplication = deliverToApplication;
  }

  /**
   * can delay to send out of order.
   */
  public broadcast(m: Message) {
    const bm: BroadcastMessage = {
      message: m,
      nodeId: this._nodeId,
      // this captures the causal dependency at the time this message was sent.
      deps: [...this._delivered],
    };
    this._delivered[this._nodeId] ??= 0;
    this._delivered[this._nodeId]++;
    return bm;
  }

  public receive(...ms: BroadcastMessage[]) {
    for (const m of ms) {
      if (m.nodeId === this._nodeId) continue;

      this._buffer.push(m);

      let found: BroadcastMessage | undefined = undefined;
      while (
        // continue only if a <= b, eg, we have sent all the dependencies
        // this message requires.
        // comparison similar to vector clock.
        (found = this._buffer.find((buffer) => {
          const max = Math.max(buffer.deps.length, this._delivered.length);
          for (let i = 0; i < max; i++) {
            const a = buffer.deps[i] ?? 0;
            const b = this._delivered[i] ?? 0;
            // this means buffer contains something this node hasn't yet received.
            if (a > b) return false;
          }
          return true;
        }))
      ) {
        this._deliverToApplication(found.message);
        this._delivered[found.nodeId] ??= 0;
        this._delivered[found.nodeId]++;
        this._buffer = this._buffer.filter((x) => x !== found);
      }
    }
  }
}

import assert from "node:assert/strict";
import { test } from "node:test";

// this one I took of straight from fifo example.
// form the perspective of causality, in this test case, sender1 and sender2 never hear
// from each other so the causal dependency is the respective sender.
test("Application receives messages from 2 nodes in order", async () => {
  const app: Message[] = [];
  const recipient = new NodeInstance((x) => app.push(x));

  const sender1 = new NodeInstance(() => {});
  const sender2 = new NodeInstance(() => {});

  const m11 = sender1.broadcast("1.1");
  const m12 = sender1.broadcast("1.2");
  recipient.receive(m11, m12);

  const delayedM21 = sender2.broadcast("2.1");
  const m22 = sender2.broadcast("2.2");
  recipient.receive(m22);

  const m13 = sender1.broadcast("1.3");
  recipient.receive(m13);

  assert.deepEqual(
    app.filter((m) => m.startsWith("1.")),
    ["1.1", "1.2", "1.3"],
  );
  assert.deepEqual(
    app.filter((m) => m.startsWith("2.")),
    [],
  );

  await Promise.resolve();

  recipient.receive(delayedM21);
  assert.deepEqual(
    app.filter((m) => m.startsWith("2.")),
    ["2.1", "2.2"],
  );
});

test("Application receives messages from multiple nodes in the correct causal order", () => {
  // from recipient's perspective
  // a sends [0,0]
  // b sends [0,0]
  // a sends [1, 1] but delayed to recipient, recipient holds [1, 1] instead of [2, 1]
  // b receives'a so it becomes [2, 1] then it emits message with [2,1] as dependency, recipient receives this but holds in buffer.
  // a message arrives, recipient can now process the last two messages from a and b
  const app: Message[] = [];
  const r = new NodeInstance((m) => {
    app.push(m);
  });
  const senderA = new NodeInstance(() => {});
  const senderB = new NodeInstance(() => {});

  const m00 = senderA.broadcast("0.0");
  senderB.receive(m00);
  r.receive(m00);
  const m10 = senderB.broadcast("1.0");
  senderA.receive(m00);
  r.receive(m10);
  assert.deepEqual(app, ["0.0", "1.0"]);

  const m01 = senderA.broadcast("0.1");
  senderB.receive(m01);
  // r.receive(m00); << does not receive, is delayed

  const m11 = senderB.broadcast("1.1");
  senderA.receive(m11);
  r.receive(m11);
  assert.deepEqual(app, ["0.0", "1.0"]);

  r.receive(m01);
  assert.deepEqual(app, ["0.0", "1.0", "0.1", "1.1"]);
});
