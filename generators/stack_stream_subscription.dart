import 'dart:async';

/// An extension to a
void main() {
  final controller = LastOnlyEmitter();

  void doSomething(num id, dynamic value) {
    print("From subscription $id");
    print(value);
  }

  late final FoundationErrorDisposer subscription1;
  late final FoundationErrorDisposer subscription2;
  late final FoundationErrorDisposer subscription3;
  late final FoundationErrorDisposer subscription4;

  subscription1 = controller.listen((value) {
    doSomething(1, value);
    subscription1.disposeCallback();
  });
  subscription2 = controller.listen((value) {
    doSomething(2, value);
    subscription2.disposeCallback();
  });
  subscription3 = controller.listen((value) {
    doSomething(3, value);
    subscription3.disposeCallback();
  });
  subscription4 = controller.listen((value) {
    doSomething(4, value);
    subscription4.disposeCallback();
  });

  Future.delayed(Duration(seconds: 1), () {
    controller.add("Should be from fourth");
  });

  Future.delayed(Duration(seconds: 2), () {
    controller.add("Should be from third");
  });

  Future.delayed(Duration(seconds: 3), () {
    controller.add("Should be from second");
  });

  Future.delayed(Duration(seconds: 4), () {
    controller.add("Shluld be from first");
  });
}

/// A simple event-emitter that only emits to the last listener in the stack.
class LastOnlyEmitter<T> {
  final List<Function(T)> _callbacks = [];

  LastOnlyEmitter();

  add(T value) {
    _callbacks.last.call(value);
  }

  FoundationErrorDisposer listen(Function(T) callback) {
    _callbacks.add(callback);

    // Use the current length of the array as the id.
    final id = _callbacks.length;
    return FoundationErrorDisposer(id, () {
      _callbacks.removeAt(id - 1);
    });
  }
}

class FoundationErrorDisposer {
  final int id;
  final Function disposeCallback;

  const FoundationErrorDisposer(this.id, this.disposeCallback);

  disposeListener() {
    disposeCallback();
  }
}
