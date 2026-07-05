(() => {
  let ctx = null;
  let source = null;
  let processor = null;
  let zeroGain = null;
  let stream = null;
  let recording = false;
  let channelData = []; // array of channel buffers, each an array of Float32Array chunks
  let numChannels = 2;
  let sampleRate = 44100;

  function flattenChannel(chunks, length) {
    const out = new Float32Array(length);
    let offset = 0;
    for (const c of chunks) {
      out.set(c, offset);
      offset += c.length;
    }
    return out;
  }

  function encodeWav(channels, sampleRate) {
    const numCh = channels.length;
    const numFrames = channels[0].length;
    const bytesPerSample = 2;
    const blockAlign = numCh * bytesPerSample;
    const dataSize = numFrames * blockAlign;
    const buffer = new ArrayBuffer(44 + dataSize);
    const view = new DataView(buffer);

    const writeStr = (off, s) => {
      for (let i = 0; i < s.length; i++)
        view.setUint8(off + i, s.charCodeAt(i));
    };

    writeStr(0, "RIFF");
    view.setUint32(4, 36 + dataSize, true);
    writeStr(8, "WAVE");
    writeStr(12, "fmt ");
    view.setUint32(16, 16, true); // fmt chunk size
    view.setUint16(20, 1, true); // PCM
    view.setUint16(22, numCh, true);
    view.setUint32(24, sampleRate, true);
    view.setUint32(28, sampleRate * blockAlign, true);
    view.setUint16(32, blockAlign, true);
    view.setUint16(34, 8 * bytesPerSample, true);
    writeStr(36, "data");
    view.setUint32(40, dataSize, true);

    // interleave + convert float [-1,1] -> int16
    let offset = 44;
    for (let i = 0; i < numFrames; i++) {
      for (let ch = 0; ch < numCh; ch++) {
        let s = Math.max(-1, Math.min(1, channels[ch][i]));
        s = s < 0 ? s * 0x8000 : s * 0x7fff;
        view.setInt16(offset, s, true);
        offset += 2;
      }
    }
    return new Blob([view], { type: "audio/wav" });
  }

  window.proxy = {
    async start() {
      if (recording) {
        console.warn("Already recording.");
        return;
      }
      stream = await navigator.mediaDevices.getDisplayMedia({
        video: true,
        audio: {
          echoCancellation: false,
          noiseSuppression: false,
          autoGainControl: false,
        },
      });

      const audioTracks = stream.getAudioTracks();
      if (audioTracks.length === 0) {
        stream.getTracks().forEach((t) => t.stop());
        stream = null;
        throw new Error(
          'No audio track. In the picker choose the tab and tick "Share tab audio".',
        );
      }
      stream.getVideoTracks().forEach((t) => t.stop());
      const audioStream = new MediaStream(audioTracks);

      ctx = new AudioContext();
      sampleRate = ctx.sampleRate;
      source = ctx.createMediaStreamSource(audioStream);

      const bufferSize = 4096;
      // ScriptProcessorNode is deprecated but works everywhere and is simplest for a console snippet.
      processor = ctx.createScriptProcessor(bufferSize, 2, 2);
      channelData = [];

      processor.onaudioprocess = (e) => {
        if (!recording) return;
        const inBuf = e.inputBuffer;
        numChannels = inBuf.numberOfChannels;
        for (let ch = 0; ch < numChannels; ch++) {
          if (!channelData[ch]) channelData[ch] = [];
          // copy — the buffer is reused after this callback
          channelData[ch].push(new Float32Array(inBuf.getChannelData(ch)));
        }
      };

      // route through a zero-gain node so the graph is "live" but we don't hear a doubled copy
      zeroGain = ctx.createGain();
      zeroGain.gain.value = 0;
      source.connect(processor);
      processor.connect(zeroGain);
      zeroGain.connect(ctx.destination);

      audioTracks[0].addEventListener("ended", () => {
        if (recording) this.stop();
      });

      recording = true;
      console.log("Recording. Call proxy.stop() to finish + download.");
    },

    async stop() {
      if (!recording) {
        console.warn("Not recording.");
        return;
      }
      recording = false;

      try {
        source.disconnect();
        processor.disconnect();
        zeroGain.disconnect();
      } catch (_) {}
      if (stream) stream.getTracks().forEach((t) => t.stop());
      if (ctx) await ctx.close();

      if (!channelData[0] || channelData[0].length === 0) {
        console.warn("No audio captured.");
        return;
      }

      const totalLen = channelData[0].reduce((a, c) => a + c.length, 0);
      const channels = [];
      for (let ch = 0; ch < numChannels; ch++) {
        channels.push(flattenChannel(channelData[ch], totalLen));
      }

      const blob = encodeWav(channels, sampleRate);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = `tab-audio-${Date.now()}.wav`;
      document.body.appendChild(a);
      a.click();
      a.remove();
      setTimeout(() => URL.revokeObjectURL(url), 10000);
      console.log(
        `Saved ${a.download} (${(blob.size / 1024 / 1024).toFixed(2)} MB)`,
      );

      ctx = source = processor = zeroGain = stream = null;
      channelData = [];
    },
  };

  console.log(
    "Ready. proxy.start() to begin, proxy.stop() to stop + download.",
  );
})();
