// @ts-check

// @ts-ignore
class InterpolationVocoder extends AudioWorkletProcessor {
  playbackRate = 1;

  constructor() {
    super();

    // @ts-ignore
    const port = this.port;

    port.onmessage = (e) => {
      const { type, playbackRate } =
        /** @type {{ type: string, playbackRate: number }} */ (e.data);
      if (type === "playbackRate") {
        this.playbackRate = playbackRate;
      }
    };
  }

  /**
   *
   * @param {Float32Array[][]} inputs
   * @param {Float32Array[][]} outputs
   * @param {Record<string, Float32Array>} parameters
   * @returns boolean
   */
  process(inputs, outputs, parameters) {
    const sourceLimit = Math.min(inputs.length, outputs.length);

    for (let inputNum = 0; inputNum < sourceLimit; inputNum++) {
      const inputChannel = inputs[inputNum];
      const outputChannel = outputs[inputNum];
      const channelCount = Math.min(inputChannel.length, outputChannel.length);

      for (let channel = 0; channel < channelCount; channel++) {
        for (let frame = 0; frame < inputChannel[channel].length; frame++) {
          outputChannel[channel][frame] = inputChannel[channel][frame];
        }
      }
    }

    return true;
  }
}

// @ts-ignore
registerProcessor("InterpolationVocoder", InterpolationVocoder);

//////////////////////////////////////////////////////////// fft zone ////////////////////////////////////////////////////////////

// memoization of the reversal of different lengths.
var memoizedReversal = {};
var memoizedZeroBuffers = {};

let constructComplexArray = function (signal) {
  var complexSignal = {};

  complexSignal.real =
    signal.real === undefined ? signal.slice() : signal.real.slice();

  var bufferSize = complexSignal.real.length;

  if (memoizedZeroBuffers[bufferSize] === undefined) {
    memoizedZeroBuffers[bufferSize] = Array.apply(null, Array(bufferSize)).map(
      Number.prototype.valueOf,
      0
    );
  }

  complexSignal.imag = memoizedZeroBuffers[bufferSize].slice();

  return complexSignal;
};

let bitReverseArray = function (N) {
  if (memoizedReversal[N] === undefined) {
    let maxBinaryLength = (N - 1).toString(2).length; //get the binary length of the largest index.
    let templateBinary = "0".repeat(maxBinaryLength); //create a template binary of that length.
    let reversed = {};
    for (let n = 0; n < N; n++) {
      let currBinary = n.toString(2); //get binary value of current index.

      //prepend zeros from template to current binary. This makes binary values of all indices have the same length.
      currBinary = templateBinary.substr(currBinary.length) + currBinary;

      currBinary = [...currBinary].reverse().join(""); //reverse
      reversed[n] = parseInt(currBinary, 2); //convert to decimal
    }
    memoizedReversal[N] = reversed; //save
  }
  return memoizedReversal[N];
};

// complex multiplication
function multiply(a, b) {
  return {
    real: a.real * b.real - a.imag * b.imag,
    imag: a.real * b.imag + a.imag * b.real,
  };
}

// complex addition
let add = function (a, b) {
  return {
    real: a.real + b.real,
    imag: a.imag + b.imag,
  };
};

// complex subtraction
let subtract = function (a, b) {
  return {
    real: a.real - b.real,
    imag: a.imag - b.imag,
  };
};

// euler's identity e^x = cos(x) + sin(x)
let euler = function (kn, N) {
  let x = (-2 * Math.PI * kn) / N;
  return { real: Math.cos(x), imag: Math.sin(x) };
};

// complex conjugate
let conj = function (a) {
  a.imag *= -1;
  return a;
};

/**
 * @param {Float32Array | {real?: Float32Array, imag?: Float32Array}} signal
 * @returns {{real: Float32Array, imag: Float32Array}}
 */
function fft(signal) {
  let complexSignal = {};

  // @ts-ignore
  if (signal.real === undefined || signal.imag === undefined) {
    complexSignal = constructComplexArray(signal);
  } else {
    // @ts-ignore
    complexSignal.real = signal.real.slice();
    // @ts-ignore
    complexSignal.imag = signal.imag.slice();
  }

  const N = complexSignal.real.length;
  const logN = Math.log2(N);

  if (Math.round(logN) != logN)
    throw new Error("Input size must be a power of 2.");

  if (complexSignal.real.length != complexSignal.imag.length) {
    throw new Error("Real and imaginary components must have the same length.");
  }

  const bitReversedIndices = bitReverseArray(N);

  // sort array
  let ordered = {
    real: [],
    imag: [],
  };

  for (let i = 0; i < N; i++) {
    // @ts-ignore
    ordered.real[bitReversedIndices[i]] = complexSignal.real[i];
    // @ts-ignore
    ordered.imag[bitReversedIndices[i]] = complexSignal.imag[i];
  }

  for (let i = 0; i < N; i++) {
    complexSignal.real[i] = ordered.real[i];
    complexSignal.imag[i] = ordered.imag[i];
  }
  // iterate over the number of stages
  for (let n = 1; n <= logN; n++) {
    let currN = Math.pow(2, n);

    // find twiddle factors
    for (let k = 0; k < currN / 2; k++) {
      let twiddle = euler(k, currN);

      // on each block of FT, implement the butterfly diagram
      for (let m = 0; m < N / currN; m++) {
        let currEvenIndex = currN * m + k;
        let currOddIndex = currN * m + k + currN / 2;

        let currEvenIndexSample = {
          real: complexSignal.real[currEvenIndex],
          imag: complexSignal.imag[currEvenIndex],
        };
        let currOddIndexSample = {
          real: complexSignal.real[currOddIndex],
          imag: complexSignal.imag[currOddIndex],
        };

        let odd = multiply(twiddle, currOddIndexSample);

        let subtractionResult = subtract(currEvenIndexSample, odd);
        complexSignal.real[currOddIndex] = subtractionResult.real;
        complexSignal.imag[currOddIndex] = subtractionResult.imag;

        let additionResult = add(odd, currEvenIndexSample);
        complexSignal.real[currEvenIndex] = additionResult.real;
        complexSignal.imag[currEvenIndex] = additionResult.imag;
      }
    }
  }

  return complexSignal;
}

/**
 * @returns {number[]}
 */
function ifft(signal) {
  if (signal.real === undefined || signal.imag === undefined) {
    throw new Error("IFFT only accepts a complex input.");
  }

  const N = signal.real.length;

  var complexSignal = {
    real: [],
    imag: [],
  };

  //take complex conjugate in order to be able to use the regular FFT for IFFT
  for (let i = 0; i < N; i++) {
    let currentSample = {
      real: signal.real[i],
      imag: signal.imag[i],
    };

    let conjugateSample = conj(currentSample);
    // @ts-ignore
    complexSignal.real[i] = conjugateSample.real;
    // @ts-ignore
    complexSignal.imag[i] = conjugateSample.imag;
  }

  //compute
  // @ts-ignore
  let X = fft(complexSignal);

  //normalize
  // @ts-ignore
  complexSignal.real = X.real.map((val) => {
    return val / N;
  });

  // @ts-ignore
  complexSignal.imag = X.imag.map((val) => {
    return val / N;
  });

  // @ts-ignore
  return complexSignal.real;
}
