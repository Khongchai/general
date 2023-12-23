/**
 * ```js
 *  const maxSamples = 100;
 *  const throttle = 300;
 *  const profiler = new Profiler(maxSamples, throttle);
 *
 * profiler.start("foo");
 *
 * // do something
 *
 * profiler.average("foo");
 * ```
 */
class Profiler {
  /**
   * A map of subjects to their timeSamples and readHeads.
   *
   * The timeSample array is a circular buffer of size maxSamples.
   * @type {{
   *    [name: string]: {
   *       timeSamples: number[],
   *       readHead: number
   *   }
   * }}
   */
  #subjects;

  /**
   * Max number of samples to keep in the circular buffer.
   */
  #maxSamples;

  /**
   * @type {number} The timeout id for the throttled log.
   */
  #timeout;

  /**
   * @type {(message: string) => void} The throttled log function.
   */
  #throttledLog;

  constructor(maxSamples = 100, throttle = 300) {
    this.#subjects = {};
    this.#maxSamples = maxSamples;
    this.#throttledLog = this.#throttle(console.log, throttle);
  }

  /**
   * @param {string} name
   */
  start(name) {
    if (!this.#subjects[name]) {
      this.#subjects[name] = {
        readHead: 0,
        timeSamples: [],
      };
    }
    this.#subjects[name].timeSamples[this.#subjects[name].readHead] =
      performance.now();
  }

  /**
   *
   * Debounced logging of the average time taken for the subject.
   *
   * @param {string} name
   */
  average(name) {
    if (!this.#subjects[name]) {
      return;
    }
    const then =
      this.#subjects[name].timeSamples[this.#subjects[name].readHead];
    this.#subjects[name].timeSamples[this.#subjects[name].readHead] =
      performance.now() - then;
    this.#subjects[name].readHead =
      (this.#subjects[name].readHead + 1) % this.#maxSamples;

    let result = 0;
    for (let i = 0; i < this.#subjects[name].timeSamples.length; i++) {
      result += this.#subjects[name].timeSamples[i];
    }
    result /= this.#subjects[name].timeSamples.length;
    this.#throttledLog(`${name}: ${result.toFixed(3)} ms`);
    return;
  }

  /**
   * @param {string} name
   */
  dispose(name) {
    delete this.#subjects[name];
  }

  /**
   * @param {(foo: unknown) => unknown} callback
   * @param {number} timeout
   * @returns callback
   */
  #throttle(callback, timeout) {
    let lastRan = 0;
    return function (/** @type {any} */ ...args) {
      if (Array.isArray(args)) args = args[0];
      if (!lastRan) {
        callback(args);
        lastRan = Date.now();
      } else {
        clearTimeout(this.#timeout);
        this.#timeout = setTimeout(function () {
          if (Date.now() - lastRan >= timeout) {
            callback(args);
            lastRan = Date.now();
          }
        }, timeout - (Date.now() - lastRan));
      }
    };
  }
}
