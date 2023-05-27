import 'dart:async';

void main() {
  runZoned(() {
    final List<BigInt> ints = grabBigInts(1000000);
    Zone.current.print(ints.toString());
  },
      zoneSpecification: ZoneSpecification(
          print: (Zone self, ZoneDelegate parent, Zone zone, String line) {}));
}

List<BigInt> grabBigInts(int count) {
  return List.generate(count, (index) => BigInt.from(1) << index);
}
