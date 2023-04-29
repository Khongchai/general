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
  PrefixNode _buildPrefixSubTree(String words, PrefixNode parentNode) {
    if (words.isEmpty) return parentNode;
    PrefixNode newNode = PrefixNode(char: words[0], parent: parentNode);
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
    String writeWord(PrefixNode currentNode, String stringSoFar) {
      final newString = stringSoFar + currentNode.char;
      if (currentNode.children.isEmpty) {
        return newString + ", ";
      }

      return currentNode.children
          .map((childNode) => writeWord(childNode, newString))
          .join();
    }

    return '[${writeWord(rootNode, "")}]';
  }

  // List<String> writeWord(PrefixNode currentNode, String stringSoFar) {
  //   final newString = stringSoFar + currentNode.char;
  //   if (currentNode.children.isEmpty) {
  //     return newString + ", ";
  //   }

  //   return currentNode.children.map((childNode) => writeWord(childNode, newString)).toList();
  // }
}

class PrefixNode {
  final String char;
  final Set<PrefixNode> children = {};
  final PrefixNode? parent;

  PrefixNode({required this.char, required this.parent})
      : assert(char.length <= 1);

  void addChild(PrefixNode child) {
    children.add(child);
  }

  bool hasChild(PrefixNode prefixNode) {
    return children.contains(prefixNode);
  }

  PrefixNode getChild(PrefixNode prefixNode) {
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

// Problem: If a word is a subsequence of another word in the array, it'll just get ignored. Actually, this is because the entire thing is wrong...
void main() {
  // const words = ["cat", "co", "dog", "dot"]
  const words = [
    "chicken",
    "chic",
    "cow",
    "ants",
    "a",
    "aa",
    "aaa",
    "chip",
    "dog"
  ];
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
