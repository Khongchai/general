// A non-contrived open-closed example for myself.

/// Very simple concept, and ambiguous as well.
abstract class Dictionary<K, V> {
  V get(K key);

  void put(K key, V value);
}

/// Ahhh, clearer
class LanguageDictionary implements Dictionary<String, String?> {
  final Map<String, String?> _store = {};
  final Map<String, List<String>> _synonyms = {};

  @override
  String? get(String word) {
    return _store[word];
  }

  @override
  void put(String word, String? meaning) {
    _store[word] = meaning;
  }

  List<String> synonyms(String word) {
    return _synonyms[word] ?? [];
  }
}

abstract class VoiceOutput<T> {
  void say(String word);
}

class MockVoiceOutput extends VoiceOutput<String> {
  @override
  void say(String word) {
    print(word);
  }
}

class DigitalLanguageDictionary extends LanguageDictionary {
  final VoiceOutput _voiceOutput;

  DigitalLanguageDictionary(this._voiceOutput) : super();

  void pronounce(String word) {
    final meaning = _store[word];
    if (meaning != null) {
      _voiceOutput.say(word);
    } else {
      _voiceOutput
          .say("I'm sorry, the word $word does not exist in our dictionary.");
    }
  }
}

void main() {
  final voiceOutput = MockVoiceOutput();
  final engDict = DigitalLanguageDictionary(voiceOutput);
  engDict.put("Dog", "A mammal that has four legs");
  engDict.pronounce("Dog");
}
