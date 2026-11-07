// global counter represents global id
let counter = 0;
type Message = string;
type BroadcastMessage = {
  message: Message;
  nodeId: number;
  sendSeq: number;
};

class ReliableBroadcast {
  private _listeners = new Array<(m: BroadcastMessage) => void>();
  public broadcast(m: BroadcastMessage) {
    this._listeners.forEach((l) => l(m));
  }

  public subscribe(cb: (m: BroadcastMessage) => void): () => void {
    this._listeners.push(cb);
    return () => {
      this._listeners = this._listeners.filter((x) => x !== cb);
    };
  }
}

class NodeInstance {
  private _channel: ReliableBroadcast;
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
  public constructor(
    channel: ReliableBroadcast,
    deliverToApplication: typeof this._deliverToApplication,
  ) {
    this._channel = channel;
    this._deliverToApplication = deliverToApplication;
    this._channel.subscribe(this._receive.bind(this));
  }

  /**
   * can delay to send out of order.
   */
  public broadcast(m: Message, mode: "through" | "delay" | "drop" = "through") {
    const bm = {
      message: m,
      nodeId: this._self.nodeId,
      sendSeq: this._self.seq++,
    };
    if (mode === "through") {
      this._channel.broadcast(bm);
    } else if (mode === "delay") {
      queueMicrotask(() => {
        this._channel.broadcast(bm);
      });
    }
  }

  // The key point here is that receive keeps track of the recipient's receiving order
  // before actually delivering the message to the user.
  // Fifo guarantee local order but not the global.
  private _receive(m: BroadcastMessage) {
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

import assert from "node:assert/strict";
import { test } from "node:test";

test("Application receives messages from 2 nodes in order", async () => {
  const channel = new ReliableBroadcast();

  const app: Message[] = [];
  new NodeInstance(channel, (m) => {
    app.push(m);
  });

  const sender1 = new NodeInstance(channel, () => {});
  const sender2 = new NodeInstance(channel, () => {});

  sender1.broadcast("1.1");
  sender1.broadcast("1.2");

  sender2.broadcast("2.1", "delay");
  sender2.broadcast("2.2");

  sender1.broadcast("1.3");

  /**
   We don't test the entire apps' array at once because fifo only guarantees that a node's message arrives in order.
   IF we test the whole array, we'd expect [1.1, 1.2, 1.3, 2.1, 2.2] but in FIFO, these types of interleaving are also correct:
   1.1, 2.1, 1.2, 2.2, 1.3
  2.1, 2.2, 1.1, 1.2, 1.3
  1.1, 1.2, 2.1, 1.3, 2.2
  */
  assert.deepEqual(
    app.filter((m) => m.startsWith("1.")),
    ["1.1", "1.2", "1.3"],
  );
  assert.deepEqual(
    app.filter((m) => m.startsWith("2.")),
    [],
  );

  await Promise.resolve();

  assert.deepEqual(
    app.filter((m) => m.startsWith("2.")),
    ["2.1", "2.2"],
  );
});
