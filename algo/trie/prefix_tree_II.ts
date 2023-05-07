class Trie {
  private root: TrieNode;

  constructor() {
    this.root = new TrieNode("");
  }

  insert(word: string): void {
    let node: TrieNode = this.root;
    for (const c of word) {
      if (!node.containsKey(c)) {
        node.put(c, new TrieNode(c));
      }

      node = node.get(c)!;
    }

    node.isTerminal = true;
  }

  search(word: string): TrieNode | null {
    let node: TrieNode | null = this.root;
    for (const c of word) {
      if (!node?.containsKey(c)) {
        return null;
      } else {
        node = node.get(c);
      }
    }

    return node;
  }

  startsWith(prefix: string): boolean {
    let node: TrieNode | null = this.root;
    for (const c of prefix) {
      if (!node?.containsKey(c)) {
        return false;
      } else {
        node = node.get(c);
      }
    }

    return !!node;
  }
}

class TrieNode {
  private nodes: (TrieNode | null)[];

  isTerminal: boolean;

  char: string;

  constructor(char: string) {
    this.nodes = Array(26).fill(null);
    this.isTerminal = false;
    this.char = char;
  }

  containsKey(ch: string): boolean {
    return !!this.nodes[ch.charCodeAt(0)];
  }

  get(ch: string): TrieNode | null {
    return this.nodes[ch.charCodeAt(0)];
  }

  put(ch: string, node: TrieNode): void {
    this.nodes[ch.charCodeAt(0)] = node;
  }

  recursivelyGetAllWords(
    array: string[] = [],
    resultSoFar: string = ""
  ): string[] {
    if (this.isTerminal) {
      array.push(resultSoFar + this.char);
    }

    if (this.nodes.length === 0) {
      array.push(this.char);
      return array;
    }

    for (const node of this.nodes) {
      if (!node) continue;

      node.recursivelyGetAllWords(array, resultSoFar + this.char);
    }
    return array;
  }

  //   iterativelyGetAllWords(): string[] {
  //     if (this.isTerminal) {
  //       return [this.char];
  //     }

  //     const returnArray: string[] = [];

  //     const stack: TrieNode[][] = [
  //       this.nodes.filter((node) => node !== null) as TrieNode[],
  //     ];

  //     while (stack.length > 0) {
  //       const nodes = stack.pop();

  //       for (const node of nodes) {
  //         const array: TrieNode[] = [];
  //         node?.nodes.forEach((n) => n && array.push(n));
  //         queue.push(array);
  //       }
  //     }

  //     return returnArray;
  //   }
}

function test() {
  const words = [
    "avalanche",
    "ant",
    "copious",
    "con",
    "continue",
    "dub",
    "dubious",
    "z",
    "sassy",
    "harmony",
    "paradise",
    "flourish",
    "shimmer",
    "luminous",
    "delicate",
    "ravishing",
    "whimsical",
    "lullaby",
    "cherish",
    "ecstatic",
    "vivacious",
    "radiant",
    "majestic",
    "nostalgia",
    "enchanting",
    "melancholy",
    "euphoria",
    "blissful",
    "serendipity",
    "happiness",
    "reverie",
    "ethereal",
    "graceful",
    "tranquil",
    "effervescent",
    "gossamer",
    "mystical",
    "felicity",
    "idyllic",
  ];

  const trie = new Trie();
  // Why tf can't we use constructor tear-offs!?
  words.forEach((word) => {
    trie.insert(word);
  });

  const foundNode = trie.search("e");
  console.log(foundNode!.recursivelyGetAllWords());
}

test();
