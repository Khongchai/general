import 'dart:async';

void main() {
    /// Four main stream classes -> https://dart.academy/streams-and-sinks-in-dart-and-flutter/ 
    /// Stream: This class represents an asynchronous stream of data. Listeners can subscribe to be notified of the arrival of new data events.
    /// EventSink: A sink is like a stream that flows in the opposite direction. Adding data events to an EventSink funnels that data into a connected stream.
    /// StreamController: A StreamController simplifies stream management, automatically creating a stream and sink, and providing methods for controlling a stream's behavior.
    /// StreamSubscription: Listeners on a stream can save a reference to their subscription, which will allow them to pause, resume, or cancel the flow of data they receive.

   final streamDisambiguation = StreamDisambiguation(); 

   streamDisambiguation.multiSub();
}

class StreamDisambiguation {
    /// Listening twice on a single sub will throw 
    ///  StateError("Stream has already been listened to.")
    void singleSub() {
        final controller = StreamController<String>();

        controller.stream.listen((String data) {
            print(data);
        });
        controller.stream.listen((String data) {
            print(data);
        });

        controller.sink.add("Data!");
    }

    void multiSub() {

      final controller = StreamController<String>.broadcast();

      controller.stream.listen((String data) {
          print(data);
      });
      controller.stream.listen((String data) {
          print(data);
      });

      controller.sink.add("Data!");

    }


}