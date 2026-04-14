# Notes

## What the emscripten JS glue (hello.js) is

The wasm binary is the actual compiled C — real machine code, executed natively by the browser's WebAssembly engine. But wasm on its own is extremely minimal: linear memory + arithmetic + function calls. No stdout, no files, no DOM, no malloc, no syscalls.

The JS glue file is libc + "the OS", reimplemented on top of browser APIs. It provides:

- **HEAP8/16/32/F32/...** — typed-array views over wasm's linear memory so JS can read/write the same bytes C sees.
- **`_fd_write`** — WASI syscall shim. `printf` → `fwrite` → `fd_write` import → this JS function → `console.log`.
- **FS stubs** — fake filesystem (MEMFS/NODEFS). Stubbed to errors if you don't use files.
- **Browser / WebGL / audio shims** — translate GL/audio calls from C into WebGL / Web Audio calls.
- **Bootstrap plumbing** — environment detection, fetching + instantiating `hello.wasm`, stack cookies, exit handling.

## Entry point

Bottom of `hello.js`:
1. `createWasm()` — async fetch + `WebAssembly.instantiate`, wiring JS shims as imports.
2. `run()` — JS "main". Waits for wasm load, then `doRun()`.
3. `initRuntime()` — calls wasm's `__wasm_call_ctors` (C++ global ctors).
4. `callMain()` → `_main` — your C `main()`, living in wasm. **This is the real entry.**

## Shared memory (the key insight)

A wasm module has a `WebAssembly.Memory`, backed by a single `ArrayBuffer`. That buffer **is** the C program's entire address space — stack, heap, globals, string literals all live inside it.

Pointers in C are just integer offsets into that buffer. `HEAPU8[ptr]` in JS reads the exact byte C would read at `*ptr`. No copy — same bytes, two views.

### `fd_write` walkthrough

C's `printf("Hello, world!\n")` → `fd_write(1, iov, 1, &written)` where `iov` is a pointer to `struct { char* base; size_t len; }` sitting in wasm memory.

Wasm can't hand JS a struct, so it passes the integer address. JS then:

```js
var ptr = HEAPU32[iov >> 2];      // iov->base
var len = HEAPU32[(iov+4) >> 2];  // iov->len
```

`>> 2` because `HEAPU32` is indexed in 4-byte words but `iov` is a byte address. JS walks `HEAPU8[ptr+j]` one byte at a time, feeds to `console.log`, writes the "bytes written" count back to `HEAPU32[pnum >> 2]` so C sees it on return.

## The memory doesn't live "in JS"

Common misconception: "wasm asks JS for memory." Wrong.

- `WebAssembly.Memory` is allocated by the **engine** (V8/SpiderMonkey) as raw OS memory, like any malloc.
- JS has an `ArrayBuffer` handle + typed-array views into it.
- Wasm has direct access to the same bytes via `i32.load`/`i32.store` — compiled to native `mov` instructions.

Think two processes sharing memory via `mmap`. Neither "owns" it more; the engine does.

When wasm runs a hot loop over an array:
- Engine executes native CPU instructions against the shared region.
- JS is not invoked, not awake, not involved.
- The only time JS runs is when wasm `call`s an **imported** function (the only exit from wasm).

Mental model: wasm is native code running on the CPU with its own memory region. JS happens to also have a view, for interop. JS is the secretary handling phone calls to the DOM/console/fetch — wasm is doing the compute.

## Where a big C array lives

All three regions live inside the same linear-memory ArrayBuffer:

1. **Stack** — `int arr[1000]` inside a function. Allocated by decrementing wasm's stack pointer. Default stack is tiny (**64 KB**). Big arrays here → stack overflow.
2. **Static / .data / .bss** — globals, `static` arrays. Fixed addresses assigned at link time; initialized from the wasm data section or zero-init. Free at runtime.
3. **Heap** — `malloc`/`new`. Managed by an allocator (dlmalloc/emmalloc) written in C and compiled into your wasm. Returns integer addresses. If heap is exhausted, calls `memory.grow` to extend the ArrayBuffer in 64KB pages — views go stale, need re-creating via `updateMemoryViews()`.

Rough layout:
```
[ static data | stack (grows down) | heap (grows up) → can grow → ]
```

Pointer arithmetic + loads/stores are pure wasm instructions — no boundary crossing. Sweet spot for wasm: big numeric array + tight loop stays entirely inside wasm. JS never needs to touch it unless you ask.

To share with JS zero-copy:
```js
new Int32Array(wasmMemory.buffer, ptr, length)
```
Careful: if wasm grows memory afterward, the view detaches and needs refreshing.

## When JS can be faster than C→wasm→JS

Wasm is fast *in the middle*, slow *at the edges*. Every boundary crossing costs real time.

Cases where pure JS beats wasm:

- **DOM/Web API–heavy.** Each call is wasm → JS shim → API. Pure JS skips the shim.
- **String-heavy.** C strings are UTF-8 bytes in the heap; JS strings are UTF-16 objects. Crossings need encode/decode (`UTF8ToString`, `stringToUTF8`).
- **Small, frequent calls.** Engines can inline pure-JS calls; they can't inline across the wasm boundary the same way.
- **Allocation-heavy, short-lived objects.** JS's GC'd allocator + generational GC often beats wasm's userspace `malloc` for churn.

Where wasm wins: pure number crunching that stays inside wasm — codecs, physics, crypto, parsers, image processing, chess engines. Rule of thumb: **minimize edge crossings**. The longer wasm runs without calling out, the bigger the win.
