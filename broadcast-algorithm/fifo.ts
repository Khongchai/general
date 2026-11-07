// global counter represents global id
let counter = 0;
type Message = string;
type BroadcastMessage = {
  message: Message;
  nodeId: number;
  sendSeq: number;
};

class NodeInstance {
  private _self = {
    nodeId: counter++,
    seq: 0,
  };

  // keyed by other node's instance id
  private _delivery: Record<
    number,
    {
      // ascending-sort buffer
      buffer: BroadcastMessage[];
      // expected sequence
      expectedSeq: number;
    }
  > = {};
  private _deliverToApplication: (m: string) => void;
  public constructor(deliverToApplication: typeof this._deliverToApplication) {
    this._deliverToApplication = deliverToApplication;
  }

  /**
   * can delay to send out of order.
   */
  public broadcast(m: Message): BroadcastMessage {
    const bm = {
      message: m,
      nodeId: this._self.nodeId,
      sendSeq: this._self.seq++,
    };
    return bm;
  }

  // The key point here is that receive keeps track of the recipient's receiving order
  // before actually delivering the message to the user.
  // Fifo guarantee local order but not the global.
  public receive(...ms: BroadcastMessage[]) {
    for (const m of ms) {
      if (m.nodeId === this._self.nodeId) return;

      const d = (this._delivery[m.nodeId] ??= {
        buffer: [],
        expectedSeq: 0,
      });

      d.buffer.push(m);
      d.buffer.sort((a, b) => a.sendSeq - b.sendSeq);
      for (const b of d.buffer) {
        if (d.expectedSeq !== b.sendSeq) continue;
        this._deliverToApplication(b.message);
        d.expectedSeq++;
      }
    }
  }
}

import assert from "node:assert/strict";
import { test } from "node:test";

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
  /**
   We don't test the entire apps' array at once because fifo only guarantees that a node's message arrives in order.
   IF we test the whole array, we'd expect [1.1, 1.2, 1.3, 2.1, 2.2] but in FIFO, these types of interleaving are also correct:
   1.1, 2.1, 1.2, 2.2, 1.3
  2.1, 2.2, 1.1, 1.2, 1.3
  1.1, 1.2, 2.1, 1.3, 2.2
  */
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
