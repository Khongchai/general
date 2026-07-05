(() => {
  let mediaRecorder = null;
  let chunks = [];
  let stream = null;

  window.proxy = {
    async start() {
      if (mediaRecorder && mediaRecorder.state === "recording") {
        console.warn("Already recording.");
        return;
      }
      // Video must be requested for the "Share tab audio" checkbox to appear.
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

      // We only want audio — drop the video track.
      stream.getVideoTracks().forEach((t) => t.stop());
      const audioStream = new MediaStream(audioTracks);

      chunks = [];
      const mime = MediaRecorder.isTypeSupported("audio/webm;codecs=opus")
        ? "audio/webm;codecs=opus"
        : "audio/webm";

      mediaRecorder = new MediaRecorder(audioStream, { mimeType: mime });
      mediaRecorder.ondataavailable = (e) => {
        if (e.data && e.data.size > 0) chunks.push(e.data);
      };
      mediaRecorder.onstop = () => {
        const blob = new Blob(chunks, { type: mime });
        const url = URL.createObjectURL(blob);
        const a = document.createElement("a");
        a.href = url;
        a.download = `tab-audio-${Date.now()}.webm`;
        document.body.appendChild(a);
        a.click();
        a.remove();
        setTimeout(() => URL.revokeObjectURL(url), 10000);
        console.log(
          `Saved ${a.download} (${(blob.size / 1024).toFixed(1)} KB)`,
        );
      };

      // If the user ends sharing via the browser's own bar, stop cleanly.
      audioTracks[0].addEventListener("ended", () => {
        if (mediaRecorder && mediaRecorder.state === "recording") this.stop();
      });

      mediaRecorder.start();
      console.log("Recording. Call proxy.stop() to finish + download.");
    },

    stop() {
      if (!mediaRecorder || mediaRecorder.state !== "recording") {
        console.warn("Not recording.");
        return;
      }
      mediaRecorder.stop();
      if (stream) stream.getTracks().forEach((t) => t.stop());
      stream = null;
    },
  };

  console.log(
    "Ready. proxy.start() to begin, proxy.stop() to stop + download.",
  );
})();
