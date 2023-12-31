let sharedBuffer;
let countIndex;

/**
 * @param {MessageEvent<
 *  { sharedBuffer: SharedArrayBuffer; countIndex: number } | "increase"
 * >} e
 */
onmessage = function (e) {
  if (e.data.sharedBuffer) {
    // Initialize shared buffer and count index
    sharedBuffer = new Int32Array(e.data.sharedBuffer);
    countIndex = e.data.countIndex;
  } else if (e.data === "increase") {
    // Increase the count and post the updated count back to the main thread
    Atomics.add(sharedBuffer, countIndex, 1);
    Atomics.notify(sharedBuffer, countIndex); // Notify any waiting threads
    postMessage({
      newCount: sharedBuffer[countIndex],
    });
  }
};
