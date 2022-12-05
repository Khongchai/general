// Group1 and Group2 are the same, notice that to scope the async code, with await,
// we have to create another function whereas promise is concise and do everything
// within the same function.

import "dart:async";

class Group1 {
  Future<String> _someFutureResult() {
    final c = Completer<String>();
    // complete will be called in 3 seconds by the timer.
    Timer(Duration(seconds: 3), () => c.complete("you should see me second"));
    return c.future;
  }

  _anotherAsyncFunction() async {
    final res = await _someFutureResult();
    print(res);
  }

  main() {
    _anotherAsyncFunction();
    print("you should see me first");
  }
}

class Group2 {
  Future<String> _someFutureResult() {
    final c = Completer<String>();
    // complete will be called in 3 seconds by the timer.
    Timer(Duration(seconds: 3), () => c.complete("you should see me second"));
    return c.future;
  }

  main() {
    _someFutureResult().then((result) {
      print(result);
    });
  }
}
