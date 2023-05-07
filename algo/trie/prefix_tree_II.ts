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

  // What about iterative dfs?
  iterativelyGetAllWords(): string[] {
    if (this.isTerminal) {
      return [this.char];
    }

    // Map the node to a prefix string.
    const stack: { nodes: TrieNode[]; prefixSoFar: string }[] = [
      {
        nodes: this.nodes.filter((node) => node !== null) as TrieNode[],
        prefixSoFar: this.char,
      },
    ];

    const returnArray: string[] = [];

    while (stack.length > 0) {
      const { nodes, prefixSoFar } = stack.pop()!;

      for (const node of nodes) {
        const newPrefix = prefixSoFar + node.char;

        node.isTerminal && returnArray.push(newPrefix);

        // Push all children onto the stack.
        stack.push({
          nodes: node.nodes.filter((node) => node !== null) as TrieNode[],
          prefixSoFar: newPrefix,
        });
      }
    }

    return returnArray.sort();
  }
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

  const result: string[][] = [];

  const foundNode = trie.search("ef");
  console.time("Recursive");
  for (let i = 0; i < 1000000; i++) {
    result.push(foundNode!.recursivelyGetAllWords());
  }
  console.timeEnd("Recursive");
  console.time("Iterative");
  for (let i = 0; i < 1000000; i++) {
    result.push(foundNode!.iterativelyGetAllWords());
  }
  console.timeEnd("Iterative");
}

test();
