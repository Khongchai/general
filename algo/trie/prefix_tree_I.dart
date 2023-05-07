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
  AbstractNode _buildPrefixSubTree(String words, AbstractNode parentNode) {
    if (words.isEmpty) {
      return parentNode..addChild(TerminalNode(parent: parentNode));
    }
    AbstractNode newNode = PrefixNode(char: words[0], parent: parentNode);
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
    traverse(rootNode, (string) {
      words.add(string);
    });

    return '[${words.join(", ")}]';
  }

  void traverse(
      AbstractNode currentNode, void Function(String string) terminalCallback,
      {String stringSoFar = ""}) {
    if (currentNode is TerminalNode) {
      terminalCallback(stringSoFar);
      return;
    }

    final newString = stringSoFar + currentNode.char;
    currentNode.children
        .map((childNode) =>
            traverse(childNode, terminalCallback, stringSoFar: newString))
        .join();
    return;
  }
}

class TerminalNode extends AbstractNode {
  TerminalNode({required AbstractNode parent})
      : super(char: "\$", parent: parent);

  @override
  void addChild(covariant AbstractNode child) {
    throw UnimplementedError();
  }

  @override
  AbstractNode getChild(covariant AbstractNode node) {
    throw UnimplementedError();
  }

  @override
  bool hasChild(covariant AbstractNode node) {
    throw UnimplementedError();
  }
}

class PrefixNode extends AbstractNode {
  final Set<AbstractNode> children = {};

  PrefixNode({
    required String char,
    required AbstractNode? parent,
  })  : assert(char.length <= 1),
        super(char: char, parent: parent);

  void addChild(AbstractNode child) {
    children.add(child);
  }

  bool hasChild(AbstractNode prefixNode) {
    return children.contains(prefixNode);
  }

  AbstractNode getChild(AbstractNode prefixNode) {
    final child = children.lookup(prefixNode);
    assert(child != null);
    return child!;
  }

  /// Returns the root node for a given text
  ///
  /// Once you have your node, you can print or get the texts or whatever.
  PrefixNode? search(String text) {
    if (children.isEmpty) {
      throw new Exception(
          "Bro, there should always be at least 1 child (the terminal node)");
    }

    for (final child in children) {
      if (child is TerminalNode && text.isEmpty) {
        return this;
      } else if (child is TerminalNode) {
        return null;
      }

      final childNode = child as PrefixNode;
      if (childNode.char == text[0] && text.length == 1) {
        return child;
      } else {
        return childNode.search(text.substring(1));
      }
    }

    return null;
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

abstract class AbstractNode {
  final AbstractNode? parent;
  final String char;
  final Set<AbstractNode> children = {};

  AbstractNode({required this.parent, required this.char});

  // Allow covariance...just in case
  void addChild(covariant AbstractNode child);
  bool hasChild(covariant AbstractNode node);
  AbstractNode getChild(covariant AbstractNode node);
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
    final PrefixNode? answerNode = trie.rootNode.search(element.key);
    // Shouldn't be null, if null, it's wrong
    trie.traverse(answerNode!, (value) {
      // Don't add the search term to the answer.
      if (value != element.key) {
        answer.add(value);
      }
    });

    assert(
        testCases[element.key]!.length == answer.length &&
            testCases[element.key]!.toSet().difference(answer.toSet()).length ==
                0,
        "Wrong answer, expect ${testCases[element.key]}, got: $answer");
  });
  print("All tests passed!");
}
