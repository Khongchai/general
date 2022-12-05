void main() {
  final stopwatch = Stopwatch();
  stopwatch.start();
  for (int i = 0; i < 100000000; i++) {
    for (int j = 0; j < 20; j++) {}
  }
  print(stopwatch.elapsedMilliseconds);
}
