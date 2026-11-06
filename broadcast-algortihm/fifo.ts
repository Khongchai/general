// global counter represents global id
let counter = 0;
type Message = string;
type BroadcastMessage = {
  nodeId: number;
  sendSeq: number;
  message: Message;
};

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

class Participator {
  private _nodeId = counter++;
  private _seq = 0;
  private _channel = new ReliableBroadcast();

  public broadcast(m: Message) {
    this._channel.broadcast({
      message: m,
      nodeId: this._nodeId,
      sendSeq: this._seq++,
    });
  }
}

class FIFOBroadcast {
  // delivered: new Array<number>(),
  // buffer: new Array<number>(),
}
