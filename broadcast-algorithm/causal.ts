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
  private _nodeId = counter++;
  private _delivered: number[] = [];
  private _buffer: BroadcastMessage[] = [];

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
    const bm: BroadcastMessage = {
      message: m,
      nodeId: this._nodeId,
      // this captures the causal dependency at the time this message was sent.
      deps: [...this._delivered],
    };
    this._delivered[this._nodeId] ??= 0;
    this._delivered[this._nodeId]++;
    if (mode === "through") {
      this._channel.broadcast(bm);
    } else if (mode === "delay") {
      queueMicrotask(() => {
        this._channel.broadcast(bm);
      });
    }
  }

  private _receive(m: BroadcastMessage) {
    if (m.nodeId === this._nodeId) return;

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
  assert.deepEqual(
    app.filter((m) => m.startsWith("1.")),
    ["1.1", "1.2"],
  );

  sender2.broadcast("2.1", "delay");
  sender2.broadcast("2.2");

  sender1.broadcast("1.3");

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

test("Application receives messages from multiple nodes in the correct causal order", () => {});
