// @ts-check

class StretchCompressProcessor extends AudioWorkletProcessor {
  constructor() {
    super();

    this.port.onmessage = (event) => {
      if (event.data.type === "play") {
        this.playing = true;
      } else if (event.data.type === "pause") {
        this.playing = false;
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

registerProcessor("stretchCompressProcessor", StretchCompressProcessor);
