// @ts-check

// TODO turn audioBuffer from array of float32 into one long array so that we  can do .subarray when we use the windowing in the synthesisframe view.
// TODO optimize all variable allocations.

const { floor, ceil, max } = Math;

const webAudioBlockSize = 128;

// @ts-ignore
class InterpolationVocoder extends AudioWorkletProcessor {
  /**
   * @type {number}
   */
  playbackRate;

  /**
   * @type {number}
   */
  numberOfInputs;

  /**
   * @type {number}
   */
  hopSize;

  /**
   * @type {number}
   */
  synthesisFrameSize;

  /**
   * @type {number}
   */
  analysisFrameSize;

  /**
   * @type {{
   *  leftFrame: Float32Array;
   *  rightFrame: Float32Array;
   * }[]}
   *
   * The buffer that holds the copy of frames to be sent to the PhaseVocoder.
   *
   * The array represents the channels.
   */
  analysisFramesToSend;

  /**
   * @type {Float32Array[]} The buffers the PhaseVocoder will write to. The result of the interpolation resampling.
   *
   * The array represents the channels.
   */
  outputBuffers;

  /**
   * @type {() => boolean}
   *
   * Returns true if the first synthesis frame has been filled. If not, don't output any sound yet.
   *
   * The array represents the the channels.
   */
  isFirstSynthesisFrameFilled;

  /**
   * No audio should be output until the vocoder is initialized.
   */
  isInitialized = false;

  /**
   * @type {number}
   *
   * TODO
   *
   * For now, we work with just one input.
   */
  inputIndex = 0;

  isPaused = true;

  synthesisFrameFilledCount = 0;

  /**
   * @type {Float32Array}
   */
  hanningWindow;

  /**
   * @type {number} The position of the current frame pointer. If the playbackRate is 0.5, this value will be [0, 0.5, 1, 1.5, ...]
   */
  currentFramePosition;
  /**
   * @type {number | null}
   */
  previousFrameFloored;

  /**
   * @type {number}
   */
  hopPerAnalysisFrame;

  /**
   * @type {number}
   */
  hopPerSynthesisFrame;

  /**
   * @type {{
   *    createComplexArray: () => number[],
   *    realTransform: (output: number[], input: number[]) => void,
   *    completeSpectrum: (input: number[]) => void
   *    inverseTransform: (output: number[], input: number[]) => number[]
   *    fromComplexArray: (input: number[], output: number[]) => void;
   * }}
   *
   * createComplexArray() returns an array of length frameSize * 2, the real and imaginary parts interleaved.
   * realTransform() performs fft on real input and stores in complex output
   * completeSpectrum() takes a complex array of size frame / 2 and fills the rest of the array with the mirrored negative frequencies.
   * inverseTransform() performs inverse on complex input and stores in complex output
   * fromComplexArray() turns a complex array input into a real array output
   *
   */
  fft;

  /**
   *  @type {{
   * currentFrameComplexArray: number[],
   * nextFrameComplexArray: number[],
   * outputComplexBuffer: number[],
   * timeComplexBuffer: number[],
   * outputBuffer: number[],
   * phaseAccumulator: Float32Array[],
   * }}
   *
   * Stuff we can reuse within each process() call
   */
  fftRecyclebin;

  /**
   * @type {{pointer: number, data: Float32Array[], forwardPointer: () => void}[]}
   *
   * audioBuffer is a circular buffer that holds the audio samples for the interpolation vocoder. When the audio playbackRate is lower, we need to store the audio samples for longer.
   * One audioReadBuffer is created for each channel.
   *
   * The length of the circular audio buffer is equal to ((1 - rate) * audioSampleLength) / frameSize
   */
  audioReadBuffer;

  /**
   * @param {{numberOfInputs: number, numberOfOutputs: number}} options
   */
  constructor(options) {
    super();

    // @ts-ignore
    const port = this.port;

    this.numberOfInputs = options.numberOfInputs;

    port.onmessage = (
      /** @type {{ data: { type: "initialize"; playbackRate: number; channelCount: number, audioDurationInSamples: number, minimumPlaybackRate: number } | {type: "playbackRate", playbackRate: string} | {type: "pause"} | {type: "resume"}}} */ e
    ) => {
      const { type } = e.data;

      switch (type) {
        case "playbackRate": {
          this.playbackRate = parseFloat(e.data.playbackRate);
          break;
        }

        case "pause": {
          this.isPaused = true;
          break;
        }

        case "resume": {
          this.isPaused = false;
          break;
        }

        // this is done just once, so let's focus on readability.
        // we have to move initialization here because we need to know the channel count.
        // some other implementations just reallocate the buffers later, but that'd be mixing the initialization and the processing logic. Me no like.
        case "initialize": {
          console.log("Initializing vocoder");
          const {
            audioDurationInSamples,
            channelCount,
            playbackRate,
            minimumPlaybackRate,
          } = e.data;
          this.playbackRate = playbackRate;

          this.hopSize = webAudioBlockSize;
          this.analysisFrameSize = 2048;
          this.synthesisFrameSize = this.analysisFrameSize + this.hopSize;

          this.analysisFramesToSend = new Array(channelCount);
          this.outputBuffers = new Array(channelCount);
          for (let channel = 0; channel < channelCount; channel++) {
            this.outputBuffers[channel] = new Float32Array(
              this.analysisFrameSize
            );

            this.analysisFramesToSend[channel] = Object.freeze({
              leftFrame: new Float32Array(this.analysisFrameSize),
              rightFrame: new Float32Array(this.analysisFrameSize),
            });
          }

          // @ts-ignore
          this.fft = new FFT(this.analysisFrameSize);

          this.synthesisFrameFilledCount = 0;
          // this will result in about 50 ms of latency for 44.1 kHz audio (`synthesisFrameSize / 44100`)
          this.isFirstSynthesisFrameFilled = () => {
            if (this.synthesisFrameFilledCount < this.synthesisFrameSize) {
              return false;
            }

            return true;
          };

          this.hopPerAnalysisFrame = this.analysisFrameSize / this.hopSize;
          if (this.hopPerAnalysisFrame % 1 !== 0) {
            throw new Error("analysisFrameSize must be divisible by hopSize");
          }
          this.hopPerSynthesisFrame = this.synthesisFrameSize / this.hopSize;
          if (this.hopPerSynthesisFrame % 1 !== 0) {
            throw new Error("synthesisFrameSize must be divisible by hopSize");
          }

          this.hanningWindow = new Float32Array(this.analysisFrameSize);
          for (let i = 0; i < this.analysisFrameSize; i++) {
            this.hanningWindow[i] =
              0.5 *
              (1 - Math.cos((2 * Math.PI * i) / (this.analysisFrameSize - 1)));
          }

          this.fftRecyclebin = {
            currentFrameComplexArray: this.fft.createComplexArray(),
            nextFrameComplexArray: this.fft.createComplexArray(),
            outputComplexBuffer: this.fft.createComplexArray(),
            timeComplexBuffer: this.fft.createComplexArray(),
            outputBuffer: new Array(this.analysisFrameSize).fill(0),
            phaseAccumulator: Array.from({ length: channelCount }, () =>
              new Float32Array(this.analysisFrameSize / 2).fill(0)
            ),
          };

          this.audioReadBuffer = [];
          const framesToBeBuffered = ceil(
            max(0, 1 - minimumPlaybackRate) *
              (audioDurationInSamples / webAudioBlockSize)
          );
          console.debug(framesToBeBuffered);
          console.debug(minimumPlaybackRate);
          console.debug(audioDurationInSamples);
          console.debug(channelCount);
          for (let i = 0; i < channelCount; i++) {
            this.audioReadBuffer[i] = {
              data: Array.from(
                { length: framesToBeBuffered },
                () => new Float32Array(webAudioBlockSize)
              ),
              forwardPointer: function () {
                this.pointer = (this.pointer + 1) % this.data.length;
              },
              pointer: 0,
            };
          }

          this.currentFramePosition = 0;
          this.previousFrameFloored = null;

          this.isInitialized = true;

          port.postMessage({ type: "initialized" });
          break;
        }

        default:
          break;
      }
    };
  }

  /**
   *
   * @param {Float32Array[][]} inputs
   * @param {Float32Array[][]} outputs
   * @returns boolean
   */
  process(inputs, outputs) {
    if (this.isPaused) {
      return true;
    }

    if (!this.isInitialized) {
      return true;
    }

    if (!this.isFirstSynthesisFrameFilled()) {
      // TODO refactor this chunk. call it preprocess();
      this.synthesisFrameFilledCount += this.hopSize;
      for (
        let channel = 0;
        channel < inputs[this.inputIndex].length;
        channel++
      ) {
        this.bufferAudioInput(inputs[this.inputIndex][channel], channel);
      }

      // if this is true, we have reached a point where we can pre-proces the
      // phase of the first analysis frame.
      if (this.synthesisFrameFilledCount === this.analysisFrameSize) {
        const wrap = this.audioReadBuffer[0].data.length;
        for (
          let channel = 0;
          channel < inputs[this.inputIndex].length;
          channel++
        ) {
          const complexArrayOut = this.fft.createComplexArray();
          const firstFrame = new Float32Array(this.analysisFrameSize);
          // fill the first frame with the data of the first analysis frame.
          for (let i = 0; i < this.hopPerAnalysisFrame; i++) {
            firstFrame.set(
              this.audioReadBuffer[channel].data[i % wrap],
              i * this.hopSize
            );
          }
          // apply hanning
          for (let i = 0; i < this.analysisFrameSize; i++) {
            firstFrame[i] *= this.hanningWindow[i];
          }
          // get the phase data.
          this.fft.realTransform(complexArrayOut, Array.from(firstFrame));
          for (let i = 0, j = 0; i < complexArrayOut.length / 2; i += 2, j++) {
            this.fftRecyclebin.phaseAccumulator[channel][j] = Math.atan2(
              complexArrayOut[i + 1],
              complexArrayOut[i]
            );
          }
        }
      }

      return true;
    }

    this.pickInputIndex();
    for (let channel = 0; channel < inputs[this.inputIndex].length; channel++) {
      this.prepareFramesToSend(channel);
      this.bufferAudioInput(inputs[this.inputIndex][channel], channel);
      this.resample(
        this.analysisFramesToSend[channel],
        this.outputBuffers[channel],
        this.analysisFrameSize,
        this.hopSize,
        channel
      );
      this.loadOutputBuffersToOutput(outputs, channel);
      this.shiftOutputBuffers(channel);
    }
    this.forwardPosition();

    // for (let input = 0; input < inputs.length; input++) {
    //   for (let channel = 0; channel < inputs[input].length; channel++) {
    //   // uncomment to map with no processing
    //     outputs[input][channel].set(inputs[input][channel]);
    //   }
    // }

    return true;
  }

  pickInputIndex(inputs) {
    // TODO (later. for now return 0)
    this.inputIndex = 0;
  }

  /**
   * @param {number} channel
   * Prepare frames to send by loading from audio buffer onto analysisFramesToSend.
   */
  prepareFramesToSend(channel) {
    const wrap = this.audioReadBuffer[0].data.length;
    const positionFloored = floor(this.currentFramePosition);
    for (let i = 0; i < this.hopPerAnalysisFrame; i++) {
      this.analysisFramesToSend[channel].leftFrame.set(
        this.audioReadBuffer[channel].data[(positionFloored + i) % wrap],
        i * this.hopSize
      );
      this.analysisFramesToSend[channel].rightFrame.set(
        this.audioReadBuffer[channel].data[(1 + positionFloored + i) % wrap],
        i * this.hopSize
      );
    }
  }

  /**
   * @param {Float32Array[][]} outputs
   * @param {number} channel
   *
   * Load the first `webAudioBlockSize` samples of the outputBuffers to the output.
   */
  loadOutputBuffersToOutput(outputs, channel) {
    outputs[this.inputIndex][channel].set(
      this.outputBuffers[channel].subarray(0, webAudioBlockSize)
    );
  }

  /**
   * Self-explanatory.
   *
   * @param {number} channel
   */
  shiftOutputBuffers(channel) {
    this.outputBuffers[channel].copyWithin(0, webAudioBlockSize);
    this.outputBuffers[channel]
      .subarray(this.analysisFrameSize - webAudioBlockSize)
      .fill(0);
  }

  /**
   * The buffering is done based on the current playbackRate
   * @param {Float32Array} input The audio input of size `webAudioBlockSize` to be buffered.
   * @param {number} channel the audio channel, eg. 0 for left, 1 for right.
   *
   * TODO @khongchai once everything works, might be better to use float32array instead of just array
   */
  bufferAudioInput(input, channel) {
    this.audioReadBuffer[channel].data[
      this.audioReadBuffer[channel].pointer
    ].set(input);
    this.audioReadBuffer[channel].forwardPointer();
  }

  /**
   *
   * @param {{leftFrame: Float32Array, rightFrame: Float32Array}} inputFrames
   * @param {Float32Array} outputBuffers
   * @param {number} frameSize
   * @param {number} hopSize
   * @param {number} channel
   *
   * Interpolate the two frames in inputFrames, and write the result to outputBuffers.
   *
   * This also takes care of overlap add.
   *
   * Operations can be done on the `inputFrames` directly, as they are a copy.
   */
  resample(inputFrames, outputBuffers, frameSize, hopSize, channel) {
    for (let i = 0; i < frameSize; i++) {
      inputFrames.leftFrame[i] *= this.hanningWindow[i];
      inputFrames.rightFrame[i] *= this.hanningWindow[i];
    }

    this.fft.realTransform(
      this.fftRecyclebin.currentFrameComplexArray,
      Array.from(inputFrames.leftFrame)
    );
    this.fft.realTransform(
      this.fftRecyclebin.nextFrameComplexArray,
      Array.from(inputFrames.rightFrame)
    );

    const nextFrameAlpha =
      this.currentFramePosition - Math.floor(this.currentFramePosition);
    const currentFrameAlpha = 1 - nextFrameAlpha;

    for (
      let i = 0, j = 0;
      i < this.fftRecyclebin.currentFrameComplexArray.length / 2;
      i += 2, j++
    ) {
      const magnitude =
        currentFrameAlpha *
          this.mag(
            this.fftRecyclebin.currentFrameComplexArray[i],
            this.fftRecyclebin.currentFrameComplexArray[i + 1]
          ) +
        nextFrameAlpha *
          this.mag(
            this.fftRecyclebin.nextFrameComplexArray[i],
            this.fftRecyclebin.nextFrameComplexArray[i + 1]
          );
      const phaseAlpha = this.shift(
        Math.atan2(
          this.fftRecyclebin.nextFrameComplexArray[i + 1],
          this.fftRecyclebin.nextFrameComplexArray[i]
        ) -
          Math.atan2(
            this.fftRecyclebin.currentFrameComplexArray[i + 1],
            this.fftRecyclebin.currentFrameComplexArray[i]
          )
      );
      this.fftRecyclebin.outputComplexBuffer[i] =
        magnitude * Math.cos(this.fftRecyclebin.phaseAccumulator[channel][j]);
      this.fftRecyclebin.outputComplexBuffer[i + 1] =
        magnitude * Math.sin(this.fftRecyclebin.phaseAccumulator[channel][j]);
      this.fftRecyclebin.phaseAccumulator[channel][j] += phaseAlpha;
    }

    this.fft.completeSpectrum(this.fftRecyclebin.outputComplexBuffer);
    this.fft.inverseTransform(
      this.fftRecyclebin.timeComplexBuffer,
      this.fftRecyclebin.outputComplexBuffer
    );
    this.fft.fromComplexArray(
      this.fftRecyclebin.timeComplexBuffer,
      this.fftRecyclebin.outputBuffer
    );

    for (let i = 0; i < frameSize; i++) {
      // reduce audio volume
      this.fftRecyclebin.outputBuffer[i] *= 0.22;
      if (i < frameSize - hopSize) {
        outputBuffers[i] +=
          this.fftRecyclebin.outputBuffer[i] * this.hanningWindow[i];
      } else {
        outputBuffers[i] =
          this.fftRecyclebin.outputBuffer[i] * this.hanningWindow[i];
      }
    }
  }

  forwardPosition() {
    this.currentFramePosition += this.playbackRate;
  }

  /**
   * @param {number} x
   * @param {number} y
   */
  mag(x, y) {
    return Math.sqrt(x * x + y * y);
  }

  /**
   * @param {number} r
   */
  shift(r) {
    while (r >= Math.PI) r -= 2 * Math.PI;
    while (r < -Math.PI) r += 2 * Math.PI;
    return r;
  }
}

// @ts-ignore
registerProcessor("InterpolationVocoder", InterpolationVocoder);

//////////////////////////////////////////////////////////// fft zone ////////////////////////////////////////////////////////////

("use strict");

class FFT {
  /**
   * @param {number} size
   */
  constructor(size) {
    this.size = size | 0;
    if (this.size <= 1 || (this.size & (this.size - 1)) !== 0)
      throw new Error("FFT size must be a power of two and bigger than 1");

    this._csize = size << 1;

    // NOTE: Use of `var` is intentional for old V8 versions
    var table = new Array(this.size * 2);
    for (var i = 0; i < table.length; i += 2) {
      const angle = (Math.PI * i) / this.size;
      table[i] = Math.cos(angle);
      table[i + 1] = -Math.sin(angle);
    }
    this.table = table;

    // Find size's power of two
    var power = 0;
    for (var t = 1; this.size > t; t <<= 1) power++;

    // Calculate initial step's width:
    //   * If we are full radix-4 - it is 2x smaller to give inital len=8
    //   * Otherwise it is the same as `power` to give len=4
    this._width = power % 2 === 0 ? power - 1 : power;

    // Pre-compute bit-reversal patterns
    this._bitrev = new Array(1 << this._width);
    for (var j = 0; j < this._bitrev.length; j++) {
      this._bitrev[j] = 0;
      for (var shift = 0; shift < this._width; shift += 2) {
        var revShift = this._width - shift - 2;
        this._bitrev[j] |= ((j >>> shift) & 3) << revShift;
      }
    }

    this._out = null;
    this._data = null;
    this._inv = 0;
  }
  fromComplexArray(complex, storage) {
    var res = storage || new Array(complex.length >>> 1);
    for (var i = 0; i < complex.length; i += 2) res[i >>> 1] = complex[i];
    return res;
  }
  createComplexArray() {
    const res = new Array(this._csize);
    for (var i = 0; i < res.length; i++) res[i] = 0;
    return res;
  }
  toComplexArray(input, storage) {
    var res = storage || this.createComplexArray();
    for (var i = 0; i < res.length; i += 2) {
      res[i] = input[i >>> 1];
      res[i + 1] = 0;
    }
    return res;
  }
  /**
   * @param {number[]} spectrum
   */
  completeSpectrum(spectrum) {
    var size = this._csize;
    var half = size >>> 1;
    for (var i = 2; i < half; i += 2) {
      spectrum[size - i] = spectrum[i];
      spectrum[size - i + 1] = -spectrum[i + 1];
    }
  }
  /**
   * @param {any} out
   * @param {any} data
   */
  transform(out, data) {
    if (out === data)
      throw new Error("Input and output buffers must be different");

    this._out = out;
    this._data = data;
    this._inv = 0;
    this._transform4();
    this._out = null;
    this._data = null;
  }
  /**
   * @param {any} out
   * @param {any} data
   */
  realTransform(out, data) {
    if (out === data)
      throw new Error("Input and output buffers must be different");

    this._out = out;
    this._data = data;
    this._inv = 0;
    this._realTransform4();
    this._out = null;
    this._data = null;
  }
  inverseTransform(out, data) {
    if (out === data)
      throw new Error("Input and output buffers must be different");

    this._out = out;
    this._data = data;
    this._inv = 1;
    this._transform4();
    for (var i = 0; i < out.length; i++) out[i] /= this.size;
    this._out = null;
    this._data = null;
  }
  // radix-4 implementation
  //
  // NOTE: Uses of `var` are intentional for older V8 version that do not
  // support both `let compound assignments` and `const phi`
  _transform4() {
    var out = this._out;
    var size = this._csize;

    // Initial step (permute and transform)
    var width = this._width;
    var step = 1 << width;
    var len = (size / step) << 1;

    var outOff;
    var t;
    var bitrev = this._bitrev;
    if (len === 4) {
      for (outOff = 0, t = 0; outOff < size; outOff += len, t++) {
        const off = bitrev[t];
        this._singleTransform2(outOff, off, step);
      }
    } else {
      // len === 8
      for (outOff = 0, t = 0; outOff < size; outOff += len, t++) {
        const off = bitrev[t];
        this._singleTransform4(outOff, off, step);
      }
    }

    // Loop through steps in decreasing order
    var inv = this._inv ? -1 : 1;
    var table = this.table;
    for (step >>= 2; step >= 2; step >>= 2) {
      len = (size / step) << 1;
      var quarterLen = len >>> 2;

      // Loop through offsets in the data
      for (outOff = 0; outOff < size; outOff += len) {
        // Full case
        var limit = outOff + quarterLen;
        for (var i = outOff, k = 0; i < limit; i += 2, k += step) {
          const A = i;
          const B = A + quarterLen;
          const C = B + quarterLen;
          const D = C + quarterLen;

          // Original values
          const Ar = out[A];
          const Ai = out[A + 1];
          const Br = out[B];
          const Bi = out[B + 1];
          const Cr = out[C];
          const Ci = out[C + 1];
          const Dr = out[D];
          const Di = out[D + 1];

          // Middle values
          const MAr = Ar;
          const MAi = Ai;

          const tableBr = table[k];
          const tableBi = inv * table[k + 1];
          const MBr = Br * tableBr - Bi * tableBi;
          const MBi = Br * tableBi + Bi * tableBr;

          const tableCr = table[2 * k];
          const tableCi = inv * table[2 * k + 1];
          const MCr = Cr * tableCr - Ci * tableCi;
          const MCi = Cr * tableCi + Ci * tableCr;

          const tableDr = table[3 * k];
          const tableDi = inv * table[3 * k + 1];
          const MDr = Dr * tableDr - Di * tableDi;
          const MDi = Dr * tableDi + Di * tableDr;

          // Pre-Final values
          const T0r = MAr + MCr;
          const T0i = MAi + MCi;
          const T1r = MAr - MCr;
          const T1i = MAi - MCi;
          const T2r = MBr + MDr;
          const T2i = MBi + MDi;
          const T3r = inv * (MBr - MDr);
          const T3i = inv * (MBi - MDi);

          // Final values
          const FAr = T0r + T2r;
          const FAi = T0i + T2i;

          const FCr = T0r - T2r;
          const FCi = T0i - T2i;

          const FBr = T1r + T3i;
          const FBi = T1i - T3r;

          const FDr = T1r - T3i;
          const FDi = T1i + T3r;

          out[A] = FAr;
          out[A + 1] = FAi;
          out[B] = FBr;
          out[B + 1] = FBi;
          out[C] = FCr;
          out[C + 1] = FCi;
          out[D] = FDr;
          out[D + 1] = FDi;
        }
      }
    }
  }
  // radix-2 implementation
  //
  // NOTE: Only called for len=4
  _singleTransform2(outOff, off, step) {
    const out = this._out;
    const data = this._data;

    const evenR = data[off];
    const evenI = data[off + 1];
    const oddR = data[off + step];
    const oddI = data[off + step + 1];

    const leftR = evenR + oddR;
    const leftI = evenI + oddI;
    const rightR = evenR - oddR;
    const rightI = evenI - oddI;

    out[outOff] = leftR;
    out[outOff + 1] = leftI;
    out[outOff + 2] = rightR;
    out[outOff + 3] = rightI;
  }
  // radix-4
  //
  // NOTE: Only called for len=8
  _singleTransform4(outOff, off, step) {
    const out = this._out;
    const data = this._data;
    const inv = this._inv ? -1 : 1;
    const step2 = step * 2;
    const step3 = step * 3;

    // Original values
    const Ar = data[off];
    const Ai = data[off + 1];
    const Br = data[off + step];
    const Bi = data[off + step + 1];
    const Cr = data[off + step2];
    const Ci = data[off + step2 + 1];
    const Dr = data[off + step3];
    const Di = data[off + step3 + 1];

    // Pre-Final values
    const T0r = Ar + Cr;
    const T0i = Ai + Ci;
    const T1r = Ar - Cr;
    const T1i = Ai - Ci;
    const T2r = Br + Dr;
    const T2i = Bi + Di;
    const T3r = inv * (Br - Dr);
    const T3i = inv * (Bi - Di);

    // Final values
    const FAr = T0r + T2r;
    const FAi = T0i + T2i;

    const FBr = T1r + T3i;
    const FBi = T1i - T3r;

    const FCr = T0r - T2r;
    const FCi = T0i - T2i;

    const FDr = T1r - T3i;
    const FDi = T1i + T3r;

    out[outOff] = FAr;
    out[outOff + 1] = FAi;
    out[outOff + 2] = FBr;
    out[outOff + 3] = FBi;
    out[outOff + 4] = FCr;
    out[outOff + 5] = FCi;
    out[outOff + 6] = FDr;
    out[outOff + 7] = FDi;
  }
  // Real input radix-4 implementation
  _realTransform4() {
    var out = this._out;
    var size = this._csize;

    // Initial step (permute and transform)
    var width = this._width;
    var step = 1 << width;
    var len = (size / step) << 1;

    var outOff;
    var t;
    var bitrev = this._bitrev;
    if (len === 4) {
      for (outOff = 0, t = 0; outOff < size; outOff += len, t++) {
        const off = bitrev[t];
        this._singleRealTransform2(outOff, off >>> 1, step >>> 1);
      }
    } else {
      // len === 8
      for (outOff = 0, t = 0; outOff < size; outOff += len, t++) {
        const off = bitrev[t];
        this._singleRealTransform4(outOff, off >>> 1, step >>> 1);
      }
    }

    // Loop through steps in decreasing order
    var inv = this._inv ? -1 : 1;
    var table = this.table;
    for (step >>= 2; step >= 2; step >>= 2) {
      len = (size / step) << 1;
      var halfLen = len >>> 1;
      var quarterLen = halfLen >>> 1;
      var hquarterLen = quarterLen >>> 1;

      // Loop through offsets in the data
      for (outOff = 0; outOff < size; outOff += len) {
        for (var i = 0, k = 0; i <= hquarterLen; i += 2, k += step) {
          var A = outOff + i;
          var B = A + quarterLen;
          var C = B + quarterLen;
          var D = C + quarterLen;

          // Original values
          var Ar = out[A];
          var Ai = out[A + 1];
          var Br = out[B];
          var Bi = out[B + 1];
          var Cr = out[C];
          var Ci = out[C + 1];
          var Dr = out[D];
          var Di = out[D + 1];

          // Middle values
          var MAr = Ar;
          var MAi = Ai;

          var tableBr = table[k];
          var tableBi = inv * table[k + 1];
          var MBr = Br * tableBr - Bi * tableBi;
          var MBi = Br * tableBi + Bi * tableBr;

          var tableCr = table[2 * k];
          var tableCi = inv * table[2 * k + 1];
          var MCr = Cr * tableCr - Ci * tableCi;
          var MCi = Cr * tableCi + Ci * tableCr;

          var tableDr = table[3 * k];
          var tableDi = inv * table[3 * k + 1];
          var MDr = Dr * tableDr - Di * tableDi;
          var MDi = Dr * tableDi + Di * tableDr;

          // Pre-Final values
          var T0r = MAr + MCr;
          var T0i = MAi + MCi;
          var T1r = MAr - MCr;
          var T1i = MAi - MCi;
          var T2r = MBr + MDr;
          var T2i = MBi + MDi;
          var T3r = inv * (MBr - MDr);
          var T3i = inv * (MBi - MDi);

          // Final values
          var FAr = T0r + T2r;
          var FAi = T0i + T2i;

          var FBr = T1r + T3i;
          var FBi = T1i - T3r;

          out[A] = FAr;
          out[A + 1] = FAi;
          out[B] = FBr;
          out[B + 1] = FBi;

          // Output final middle point
          if (i === 0) {
            var FCr = T0r - T2r;
            var FCi = T0i - T2i;
            out[C] = FCr;
            out[C + 1] = FCi;
            continue;
          }

          // Do not overwrite ourselves
          if (i === hquarterLen) continue;

          // In the flipped case:
          // MAi = -MAi
          // MBr=-MBi, MBi=-MBr
          // MCr=-MCr
          // MDr=MDi, MDi=MDr
          var ST0r = T1r;
          var ST0i = -T1i;
          var ST1r = T0r;
          var ST1i = -T0i;
          var ST2r = -inv * T3i;
          var ST2i = -inv * T3r;
          var ST3r = -inv * T2i;
          var ST3i = -inv * T2r;

          var SFAr = ST0r + ST2r;
          var SFAi = ST0i + ST2i;

          var SFBr = ST1r + ST3i;
          var SFBi = ST1i - ST3r;

          var SA = outOff + quarterLen - i;
          var SB = outOff + halfLen - i;

          out[SA] = SFAr;
          out[SA + 1] = SFAi;
          out[SB] = SFBr;
          out[SB + 1] = SFBi;
        }
      }
    }
  }
  // radix-2 implementation
  //
  // NOTE: Only called for len=4
  _singleRealTransform2(outOff, off, step) {
    const out = this._out;
    const data = this._data;

    const evenR = data[off];
    const oddR = data[off + step];

    const leftR = evenR + oddR;
    const rightR = evenR - oddR;

    out[outOff] = leftR;
    out[outOff + 1] = 0;
    out[outOff + 2] = rightR;
    out[outOff + 3] = 0;
  }
  // radix-4
  //
  // NOTE: Only called for len=8
  _singleRealTransform4(outOff, off, step) {
    const out = this._out;
    const data = this._data;
    const inv = this._inv ? -1 : 1;
    const step2 = step * 2;
    const step3 = step * 3;

    // Original values
    const Ar = data[off];
    const Br = data[off + step];
    const Cr = data[off + step2];
    const Dr = data[off + step3];

    // Pre-Final values
    const T0r = Ar + Cr;
    const T1r = Ar - Cr;
    const T2r = Br + Dr;
    const T3r = inv * (Br - Dr);

    // Final values
    const FAr = T0r + T2r;

    const FBr = T1r;
    const FBi = -T3r;

    const FCr = T0r - T2r;

    const FDr = T1r;
    const FDi = T3r;

    out[outOff] = FAr;
    out[outOff + 1] = 0;
    out[outOff + 2] = FBr;
    out[outOff + 3] = FBi;
    out[outOff + 4] = FCr;
    out[outOff + 5] = 0;
    out[outOff + 6] = FDr;
    out[outOff + 7] = FDi;
  }
}
