// global counter represents global id
let counter = 0;
type Message = string;
class BroadcastMessage {
  public constructor(
    public message: Message,
    public nodeId: number,
    public sendSeq: number,
  ) {}

  public toString(): string {
    const out = `${this.nodeId}:${this.sendSeq}:${this.message}`;
    return out;
  }
}

class ReliableBroadcast {
  private _listeners = new Array<(m: BroadcastMessage) => void>();
  public broadcast(m: BroadcastMessage) {
    this._listeners.forEach((l) => l(m));
  }

  public subscribe(cb: (m: BroadcastMessage) => void): () => void {
    this._listeners.push(cb);
    return () => {
      const filtered = this._listeners.filter((x) => x !== cb);
      return filtered;
    };
  }
}

class NodeInstance {
  private _channel = new ReliableBroadcast();
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

  public constructor(private _deliverToApplication: (m: string) => void) {
    this._channel.subscribe(this._receive.bind(this));
  }

  public broadcast(m: Message) {
    this._channel.broadcast(
      new BroadcastMessage(m, this._self.nodeId, this._self.seq++),
    );
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
    d.buffer.sort((m) => m.sendSeq);
    for (const b of d.buffer) {
      if (d.expectedSeq !== b.sendSeq) continue;
      this._deliverToApplication(b.message);
      d.expectedSeq++;
    }
  }
}
