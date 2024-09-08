/**
 * Adds hiss into each channel.
 *
 * @class SinGenerator
 * @extends AudioWorkletProcessor
 **/

class SineGenerator extends AudioWorkletProcessor {
  count = 0;
  phase = 0;

  constructor() {
    super();
  }

  static get parameterDescriptors() {
    return [
      {
        name: "gain",
        defaultValue: 0.2,
        minValue: 0,
        maxValue: 1,
      },
    ];
  }

  process(inputs, outputs, parameters) {
    if (this.count++ % 100 === 0) {
      console.log(this.count);
    }

    const output = outputs[0];
    const frequency = 440;
    const amplitude = 0.2;

    for (let channel = 0; channel < output.length; ++channel) {
      const outputChannel = output[channel];
      for (let i = 0; i < outputChannel.length; ++i) {
        this.phase += (2 * Math.PI * frequency) / sampleRate;
        outputChannel[i] = amplitude * Math.sin(this.phase);
        if (this.phase > 2 * Math.PI) {
          this.phase -= 2 * Math.PI;
        }
      }
    }

    return true;
  }
}

registerProcessor("sin-generator", SineGenerator);
