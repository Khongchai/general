import 'dart:async';
import 'dart:isolate';

class SessionManager {
  Future<String>? publicKey;
  Future<String>? privateKey;

  SessionManager();
}

final sessionManager = SessionManager();

/**
 * Expected print order for Future and isolate:
 * - Do something else 1 - 5 all at once
 * -"key pair generated"and pri and Using pub and pri together 
 * 
 * 
 * Expected print order for synchronous code (with the for loop)
 * Everything below all at once.
 * - "key pair generated"
 * - Do something else 1 - 5
 * - Using pub and pri >> this is because it's Future and finishes in the next event loop.
 */
void main() async {
  generatePublicAndPrivateKey();

  printPublicAndPrivateKey();

  print("Do something else 1");
  print("Do something else 2");
  print("Do something else 3");
  print("Do something else 4");
  print("Do something else 5");
}

void generatePublicAndPrivateKey() async {
  final Completer<String> publicKeyCompleter = Completer();
  final Completer<String> privateKeyCompleter = Completer();

  sessionManager.privateKey = privateKeyCompleter.future;
  sessionManager.publicKey = publicKeyCompleter.future;

  ///////////////////////////// Isolate /////////////////////////////////
  final p = ReceivePort();
  await Isolate.spawn((SendPort sendPort) async {
    for (int i = 0; i < 10000000000; i++) {}
    Isolate.exit(sendPort, [
      "generated public key: pub1234",
      "generated private key: pri1234",
    ]);
  }, p.sendPort);

  final result = await p.first as List<String>;
  print("Key pair generated");
  publicKeyCompleter.complete(result[0]);
  privateKeyCompleter.complete(result[1]);

  ///////////////////////////// Future /////////////////////////////////
  // await Future.delayed(Duration(seconds: 3), () {
  //   print("Key pair generated");
  //   publicKeyCompleter.complete("generated public key: pub1234");
  //   privateKeyCompleter.complete("generated private key: pri1234");
  // });

  ///////////////////////////// Synchronous /////////////////////////////////
  // for (int i = 0; i < 10000000000; i++) {}
  // print("Key pair generated");
  // publicKeyCompleter.complete("generated public key: pub1234");
  // privateKeyCompleter.complete("generated private key: pri1234");
}

void printPublicAndPrivateKey() async {
  final pub = await sessionManager.publicKey;
  final pri = await sessionManager.privateKey;
  print("Using public key: $pub");
  print("Using private key: $pri");
}
