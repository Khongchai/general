
import 'dart:async';

/// An extension to a 
void main() {
  final controller = StackedStreamManager<String>();

  void doSomething(num id, dynamic value){
    print("From subscription $id");
    print(value);
  }

  late final StreamSubscription subscription1;
  late final StreamSubscription subscription2;
  late final StreamSubscription subscription3;
  late final StreamSubscription subscription4;

  subscription1 = controller.listen((value) {
    doSomething(1, value);
    subscription1.cancel();
  });
  subscription2 = controller.listen((value) {
    doSomething(2, value);
    subscription2.cancel();
  });
  subscription3 = controller.listen((value) {
    doSomething(3, value);
    subscription3.cancel();
  });
  subscription4 = controller.listen((value) {
    doSomething(4, value);
    subscription4.cancel();
  });

  Future.delayed(Duration(seconds: 1), () {
    controller.add("Should be from fourth");
  });

  Future.delayed(Duration(seconds: 4), () {
    controller.add("Should be from third");
  });

  Future.delayed(Duration(seconds: 6), () {
    controller.add("Should be from second");
  });

  Future.delayed(Duration(seconds: 7), () {
    controller.add("Shluld be from first");
  });
}

class StackedStreamController {

}

class StackedStreamManager<T> {
  final StreamController<T> _streamController;

  StackedStreamManager(): _streamController = StreamController();

  List<Function> _listeners = [];

  StreamSubscription listen(Function(T value) callback){
    _listeners.add(callback);

    _streamController.sink.addStream(stream);
    _streamController.sink.add(event);
  }

  void add(T event) {
    _streamController.sink.add(event);
  }
}