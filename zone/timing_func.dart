import 'dart:async';

void main() {
  buffer();
}

Future<void> buffer() async {
  // This
  // final stopwatch = Stopwatch()..start();
  // await someAsyncFunction();
  // print(stopwatch.elapsed.inMilliseconds);

  // is not the same as this

  runZoned(() {
    someAsyncFunction();
  }, zoneSpecification: ZoneSpecification(runUnary:
      (Zone self, ZoneDelegate parent, Zone zone, R Function(T arg) f, T arg) {
    return;
  }));
  final stopwatch = Stopwatch()..start();
  print(stopwatch.elapsed.inMilliseconds);
}

Future<void> someAsyncFunction() async {
  await Future.delayed(Duration(milliseconds: 200))
      .then((value) => print("Done1"));
  Future.delayed(Duration(milliseconds: 2000))
      .then((value) => print("Done2")); // Not awaited
  await Future.delayed(Duration(milliseconds: 300))
      .then((value) => print("Done3"));
}
