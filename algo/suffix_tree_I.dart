// Construct prefix tree, traverse prefix tree
// First attempt at building a prefix tree and traversing it.
// https://en.wikipedia.org/wiki/Trie

import "dart:collection";
//        ""
//      /    \
//     c      d
//    / \      \
//   a   o      o
//  /          / \
// t          g   t

// Objective, print all word given a string

typedef Question = String;
typedef Answer = List<String>;

// Contains root node
class PrefixTree {
  late final PrefixNode rootNode;

  PrefixTree({required List<String> words}) {
    rootNode = PrefixNode(char: "", parent: null);
    for (final word in words) {
      _buildPrefixSubTree(word, rootNode);
    }
  }

  // Construct prefix sub tree, returns the first node.
  // Top down

  // TOOD make iterative.
  IPrefixNode _buildPrefixSubTree(String words, IPrefixNode parentNode) {
    if (words.isEmpty) {
      return parentNode..addChild(TerminalNode(parent: parentNode));
    }
    IPrefixNode newNode = PrefixNode(char: words[0], parent: parentNode);
    if (parentNode.hasChild(newNode)) {
      newNode = parentNode.getChild(newNode);
    } else {
      parentNode.addChild(newNode);
    }
    return _buildPrefixSubTree(words.substring(1), newNode);
  }

// Traverse prefix tree to print all the words.
// Returned string should be something like "['c', 'ca', 'cat', 'co']"
  @override
  String toString() {
    // TODO make iterative.
    final List<String> words = [];
    traverse(rootNode, "", (string) {
      words.add(string);
    });

    return '[${words.join(", ")}]';
  }

  void traverse(IPrefixNode currentNode, String stringSoFar,
      void Function(String string) terminalCallback) {
    if (currentNode is TerminalNode) {
      terminalCallback(stringSoFar);
      return;
    }

    final newString = stringSoFar + currentNode.char;
    currentNode.children
        .map((childNode) => traverse(childNode, newString, terminalCallback))
        .join();
    return;
  }
}

class TerminalNode extends IPrefixNode {
  TerminalNode({required IPrefixNode parent})
      : super(char: "\$", parent: parent);

  @override
  void addChild(covariant IPrefixNode child) {
    throw UnimplementedError();
  }

  @override
  IPrefixNode getChild(covariant IPrefixNode node) {
    throw UnimplementedError();
  }

  @override
  bool hasChild(covariant IPrefixNode node) {
    throw UnimplementedError();
  }
}

class PrefixNode extends IPrefixNode {
  final Set<IPrefixNode> children = {};

  PrefixNode({
    required String char,
    required IPrefixNode? parent,
  })  : assert(char.length <= 1),
        super(char: char, parent: parent);

  void addChild(IPrefixNode child) {
    children.add(child);
  }

  bool hasChild(IPrefixNode prefixNode) {
    return children.contains(prefixNode);
  }

  IPrefixNode getChild(IPrefixNode prefixNode) {
    final child = children.lookup(prefixNode);
    assert(child != null);
    return child!;
  }

  @override
  bool operator ==(Object other) {
    if (other is! PrefixNode) return false;
    return other.hashCode == hashCode;
  }

  @override
  int get hashCode {
    return char.hashCode;
  }

  @override
  String toString() {
    return char;
  }
}

abstract class IPrefixNode {
  final IPrefixNode? parent;
  final String char;
  final Set<IPrefixNode> children = {};

  IPrefixNode({required this.parent, required this.char});

  // Allow covariance...just in case
  void addChild(covariant IPrefixNode child);
  bool hasChild(covariant IPrefixNode node);
  IPrefixNode getChild(covariant IPrefixNode node);
}

// Problem: If a word is a subsequence of another word in the array, it'll just get ignored. Actually, this is because the entire thing is wrong...
void main() {
  const words = [
    "origin",
    "origami",
    "oreo",
    "o",
    "oman",
    "a",
    "apple",
    "axe",
    "under",
    "Über",
    "aufbewahren",
    "elect",
    "electronic",
  ];
  const Map<Question, Answer> testCases = {
    "o": ["origin", "origami", "oreo", "oman"],
    "origin": [],
    "ap": ["apple"],
    "ax": ["axe"],
    "x": [],
    "u": ["under"],
    "e": ["elect", "electronic"],
    "elect": ["elect", "electronic"],
  };
  final trie = PrefixTree(words: words);

  testCases.entries.forEach((element) {
    final List<String> answer = [];
    trie.traverse(trie.rootNode, "", (value) {
      answer.add(value);
    });

    assert(
        testCases[element.key]!.toSet().difference(answer.toSet()).length == 0,
        "Wrong answer, expect ${testCases[element.key]}, got: $answer");
  });
  print("All tests passed!");
}
