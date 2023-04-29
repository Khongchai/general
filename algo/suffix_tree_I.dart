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
    rootNode = PrefixNode(char: "");
    for (final word in words) {
      _buildPrefixSubTree(word, rootNode);
    }
  }

  // Construct prefix sub tree, returns the first node.
  // Top down

  // TODO correct the construction, it's kind of wrong?
  PrefixNode _buildPrefixSubTree(String words, PrefixNode parentNode) {
    if (words.isEmpty) return parentNode;
    final newNode = PrefixNode(char: words[0]);
    if (!parentNode.hasChild(newNode)) {
      parentNode.addChild(newNode);
    }
    return _buildPrefixSubTree(words.substring(1), newNode);
  }

// Traverse prefix tree to print all the words.
// Returned string should be something like "['c', 'ca', 'cat', 'co']"
  @override
  String toString() {
    late PrefixNode node = rootNode;

    String returnWord(PrefixNode node) {
      if (node.children.isEmpty) {
        return node.char + ",";
      }
      return node.char +
          node.children.map((element) => returnWord(element)).join("");
    }

    return "[${returnWord(node)}]";
  }
}

class PrefixNode {
  final String char;
  final Set<PrefixNode> children = {};

  PrefixNode({required this.char}) : assert(char.length <= 1);

  void addChild(PrefixNode child) {
    children.add(child);
  }

  bool hasChild(PrefixNode prefixNode) {
    return children.contains(prefixNode);
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
}

void main() {
  const words = ["cat", "co", "dog", "dot"];
  const Map<Question, Answer> testCases = {
    "": ["cat", "ca", "c", "co", "d", "do", "dog", "dot"],
    "ca": ["t"],
    "dog": [],
    "d": ["dog", "dot", "do"],
    "abcdefg": [],
  };
  final trie = PrefixTree(words: words);
  print(trie);
  // testCases.entries.forEach((element) {
  //   final answer = trie.getAllWords(element.key);
  //   assert(
  //       testCases[element.key]!.toSet().difference(answer.toSet()).length == 0,
  //       "Wrong answer, expect ${testCases[element.key]}, got: $answer");
  // });
}
