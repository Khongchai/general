// @ts-check

/**
 * @type {AudioContext | null}
 */
let audioContext = null;
/**
 * @type {HTMLInputElement | null}
 */
let hissGainRange;
/**
 * @type {HTMLInputElement | null}
 */
let oscGainRange;
/**
 * @type {AudioWorkletNode | null}
 */
let hissGenNode = null;
/**
 * @type {GainNode | null}
 */
let gainNode = null;
/**
 * @type {AudioParam  | undefined}
 */
let hissGainParam = undefined;
/**
 * @type {AudioWorkletNode | null}
 */
let sinGeneratorNode = null;
/**
 * @type {OscillatorNode | null}
 */
let oscillatorNode = null;
let playingHiss = false;

/**
 *
 * @param {AudioContext} audioContext
 * @param {string} name
 * @returns
 */
async function createProcessor(audioContext, name) {
  let processorNode;

  try {
    processorNode = new AudioWorkletNode(audioContext, name);
  } catch (e) {
    try {
      console.log("adding...");
      await audioContext.audioWorklet.addModule(name + ".js");
      processorNode = new AudioWorkletNode(audioContext, name);
    } catch (e) {
      console.log(`** Error: Unable to create worklet node: ${e}`);
      return null;
    }
  }

  await audioContext.resume();
  return processorNode;
}

async function createStuff() {
  audioContext ??= new AudioContext();

  hissGenNode = await createProcessor(audioContext, "hiss-generator");
  if (!hissGenNode) {
    console.log("** Error: unable to create hiss processor");
    return;
  }

  gainNode = audioContext.createGain();

  // Configure the oscillator node
  oscillatorNode = new OscillatorNode(audioContext);
  oscillatorNode.type = "square";
  oscillatorNode.frequency.setValueAtTime(440, audioContext.currentTime); // (A4)

  // Configure the gain for the oscillator

  gainNode.gain.setValueAtTime(
    Number(oscGainRange?.value) || 0,
    audioContext.currentTime
  );

  // Get access to the worklet's gain parameter

  hissGainParam = hissGenNode.parameters.get("gain");
  if (hissGainParam != null) {
    hissGainParam.setValueAtTime(
      Number(hissGainRange?.value) || 0,
      audioContext.currentTime
    );
  }

  sinGeneratorNode = await createProcessor(audioContext, "sin-generator");

  oscillatorNode.start();
}

window.addEventListener("load", () => {
  const toggle = document.getElementById("toggle");
  if (toggle) {
    toggle.addEventListener("click", toggleSound);
  }

  hissGainRange = /** @type {HTMLInputElement} */ (
    document.getElementById("hiss-gain")
  );
  oscGainRange = /** @type {HTMLInputElement} */ (
    document.getElementById("osc-gain")
  );

  if (hissGainRange) {
    hissGainRange.oninput = updateHissGain;
    hissGainRange.disabled = true;
  }
  if (oscGainRange) {
    oscGainRange.oninput = updateOscGain;
    oscGainRange.disabled = true;
  }
});

async function toggleSound() {
  if (
    !(
      oscillatorNode &&
      sinGeneratorNode &&
      gainNode &&
      hissGenNode &&
      audioContext
    )
  ) {
    await createStuff();
    toggleSound();
    return;
  }

  playingHiss = !playingHiss;
  if (playingHiss) {
    console.log("Hissing");

    if (sinGeneratorNode) {
      sinGeneratorNode.disconnect();
    }

    // pipeline 1
    oscillatorNode
      .connect(gainNode)
      .connect(hissGenNode)
      .connect(audioContext.destination);
  } else {
    console.log("Sining");

    if (hissGenNode) {
      hissGenNode.disconnect();
    }

    // pipeline 2
    sinGeneratorNode.connect(audioContext.destination);
  }
}

function updateHissGain(event) {
  if (!hissGainParam || !audioContext) {
    return;
  }
  hissGainParam.setValueAtTime(event.target.value, audioContext.currentTime);
}

function updateOscGain(event) {
  if (!gainNode || !audioContext) {
    return;
  }
  gainNode.gain.setValueAtTime(event.target.value, audioContext.currentTime);
}
