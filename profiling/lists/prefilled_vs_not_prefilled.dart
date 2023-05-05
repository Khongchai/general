import 'dart:math';

/// Result: {
///   "With forEach": {"10": "2 seconds"},
///   "With for of": {...},
///   ...
/// }
///
///
void main() {
  profile(listSizes: [
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
  ], profilers: [
    (arraySize, s) {
      s.start();

      final List<double> list = List.filled(arraySize, 0);
      for (int i = 0; i < arraySize; i++) {
        list[i] = i + 20 * sqrt(200 * 200);
      }

      s.stop();

      trickCompilerHaha(list);

      return "Pre-generated list";
    },
    (arraySize, s) {
      s.start();

      final List<double> list = [];
      for (int i = 0; i < arraySize; i++) {
        list.add(i + 20 * sqrt(200 * 200));
      }

      s.stop();

      trickCompilerHaha(list);

      return "Empty list";
    }
  ], repeat: 5);
}

/// Use stopwatch to profile. Return the name of this callback.
typedef Profiler = String Function(int size, Stopwatch stopwatch);

/// Returns how much time each of the list size take.
///
/// Repeat for `repeat` times and return the weighted result.
void profile(
    {required List<int> listSizes,
    required List<Profiler> profilers,
    required int repeat}) {
  assert(repeat >= 0);
  final stopwatch = Stopwatch();
  final Map<String, Map<String, Average>> profilersResultMap = {};

  do {
    for (final profiler in profilers) {
      for (final listSize in listSizes) {
        final profilerName = profiler(listSize, stopwatch);
        if (profilersResultMap[profilerName] == null) {
          profilersResultMap[profilerName] = {};
        }
        final map = profilersResultMap[profilerName]!;
        map[listSize.toString()] ??= Average();
        map[listSize.toString()]!.add(stopwatch.elapsed.inMicroseconds / 1000);
        stopwatch.reset();
      }
    }
  } while (repeat-- != 0);

  if (repeat > 0) {
    print("Weighted result across ${repeat + 1} runs: $profilersResultMap");
  } else {
    print("Result: $profilersResultMap");
  }
}

String trickCompilerHaha(dynamic anything) {
  final string = anything.toString();
  return string;
}

class Average {
  final List<double> numbers = [];

  Average();

  void add(double number) {
    numbers.add(number);
  }

  @override
  String toString() {
    if (numbers.isEmpty) return "0";
    if (numbers.length == 1) return numbers.first.toString();

    return (numbers.reduce((value, element) => value + element) /
            numbers.length)
        .toStringAsFixed(3);
  }
}
