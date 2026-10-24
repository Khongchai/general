export class LamportTimestamp {
  /**
     For simplicity, this represents random, comparable node id.
    */
  private static _globalCounter = 0;
  public clock: number;
  public nodeId: number;
  public constructor(clock: number, nodeId?: number) {
    this.clock = clock;
    this.nodeId = nodeId ?? LamportTimestamp._globalCounter++;
  }

  // https://en.wikipedia.org/wiki/Lamport_timestamp#Implications
  // Lamport timestamp can be forced to be totally ordered, but
  // true concurrency can be lost since they rely on only one scalar and
  // are tie-broken by the nodeId instead of real counters like vector clocks.
  public compare(other: LamportTimestamp): "before" | "after" | "equal" {
    if (this.clock < other.clock) return "before";
    if (this.clock > other.clock) return "after";
    if (this.nodeId < other.nodeId) return "before";
    if (this.nodeId > other.nodeId) return "after";
    // same event since both clock and nodeId are equal.
    return "equal";
  }

  public increment() {
    return new LamportTimestamp(this.clock + 1, this.nodeId);
  }

  public merge(other: LamportTimestamp): LamportTimestamp {
    const nextClock = Math.max(other.clock, this.clock);
    return new LamportTimestamp(nextClock, this.nodeId);
  }

  public receive(other: LamportTimestamp): LamportTimestamp {
    return this.merge(other).increment();
  }
}

export class VectorClock extends Array<number> {
  // https://en.wikipedia.org/wiki/Vector_clock#Partial_ordering_property
  public compare(
    other: VectorClock,
  ): "before" | "after" | "concurrent" | "equal" {
    if (other.some((x) => x < 0)) {
      throw new Error("WTF");
    }
    const n = Math.max(this.length, other.length);
    let aSmaller = false;
    let aBigger = false;

    for (let i = 0; i < n; i++) {
      const a = this[i] ?? 0;
      const b = other[i] ?? 0;
      if (a < b) aSmaller = true;
      if (a > b) aBigger = true;
    }

    if (aSmaller && aBigger) return "concurrent"; // [1,1,0] vs [0,0,2]
    // this is actually allASmallerOrEqual AND someADefinitely smaller definition from wikipedia's "Partial ordering property" section
    if (aSmaller) return "before"; // [1,0,0] vs [2,2,0]
    if (aBigger) return "after"; // [1,1,1] vs [0,0,0]
    return "equal"; // [0,0,0] vs [0,0,0]
  }

  // called when experiencing local event and when receiving event from another clock
  public increment(nodeIndex: number): VectorClock {
    const max = Math.max(nodeIndex + 1, this.length);
    const out = new VectorClock(max);
    for (let i = 0; i < max; i++) {
      out[i] = this[i] ?? 0;
    }
    out[nodeIndex] += 1;
    return out;
  }

  public merge(other: VectorClock): VectorClock {
    const n = Math.max(this.length, other.length);
    const out = new VectorClock(n);
    for (let i = 0; i < n; i++) {
      out[i] = Math.max(this[i] ?? 0, other[i] ?? 0);
    }
    return out;
  }

  public receive(other: VectorClock, nodeIndex: number): VectorClock {
    return this.merge(other).increment(nodeIndex);
  }
}

// case 1:
{
  {
    // can actually say events are concurrent (compare this with lamport timestamp, that one can't).
    const a = new VectorClock(0, 0).increment(0);
    const b = new VectorClock(0, 0).increment(1);
    // in real-life, if concurrent, resolve the conflict somehow before calling a.merge(b)
    console.info(`Comparison result: ${a.compare(b)}`);
    console.info(`Merge result ${a.merge(b)}`);
  }

  {
    // vector clock would have given [1,0] and [0,1] and outputs "concurrent". Lamport timestamp can't do that.
    const a = new LamportTimestamp(0).increment();
    const b = new LamportTimestamp(0).increment();
    console.info(`Comparison result: ${a.compare(b)}`);
  }
}
