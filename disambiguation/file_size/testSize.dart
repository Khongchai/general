import 'dart:convert';
import 'dart:io';

void main() {
  final file = File("testSize.json");
  final fileSizeInBytes = file.lengthSync();
  print("Dart: file size:  ${fileSizeInBytes}");

  final fileContent = file.readAsStringSync();
  final bytes = utf8.encode(fileContent);
  print("Dart: string size: ${bytes.length}");
}
