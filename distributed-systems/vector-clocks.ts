class VectorClock extends Array<number> {
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
    if (aSmaller) return "before"; // [1,0,0] vs [2,2,0]
    if (aBigger) return "after"; // [1,1,1] vs [0,0,0]
    return "equal"; // [0,0,0] vs [0,0,0]
  }

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

const a = new VectorClock(0, 1, 0);
const b = new VectorClock(0, 0, 1);
// in real-life, if concurrent, resolve the conflict somehow before calling a.merge(b)
console.info(`Comparison result: ${a.compare(b)}`);
console.info(`Merge result ${a.merge(b)}`);
