Just testing decoding file size between Dart, Kotlin, and TypeScript (deno).

```bat
sh testAll.sh
```

The length of characters in the testSize.json is not the same as the amount of bytes it contains.

This means that the program that attempts to read this in bytes must use the correct UTF-8 decoding to decode the content of the file when checking its size.

Steps:

1. Read file as bytes.
2. Check size of bytes.
3. Get file content as String.
4. Convert string to utf-8 bytes.
5. Check size of bytes.
