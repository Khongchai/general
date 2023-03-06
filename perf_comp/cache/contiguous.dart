/// The first one is faster by almost 1000%.
void main() {
  int sum = 0;
  List<int> arr = List.filled(4000 * 4000 + 4000, 0, growable: false);
  final stopwatch = Stopwatch();

  stopwatch.start();
  for (int i = 0; i < 4000; ++i) {
    for (int j = 0; j < 4000; ++j) {
      sum += arr[i * 4000 + j];
    }
  }
  print("Elapsed first: " + stopwatch.elapsed.toString());

  sum = 0;
  stopwatch
    ..reset()
    ..start();
  for (int i = 0; i < 4000; ++i) {
    for (int j = 0; j < 4000; ++j) {
      sum += arr[i + 4000 * j];
    }
  }
  print("Elapsed second: ${stopwatch.elapsed.toString()}");
  print("Sum: $sum");
}
