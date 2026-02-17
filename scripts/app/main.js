// main.js

let mediaRecorder = null;
let audioChunks = [];
let audioStream = null;
let audioContext = null;
let videoEl = null;
let requestMicBtn = null;
let startTaskBtn = null;
let statusEl = null;
let downloadLink = null;
let formatSelect = null;

// Hardcoded task video filename (must be in same folder)
const TASK_VIDEO_FILE = "task_video.mp4";

document.addEventListener("DOMContentLoaded", () => {
  videoEl = document.getElementById("taskVideo");
  requestMicBtn = document.getElementById("requestMic");
  startTaskBtn = document.getElementById("startTask");
  statusEl = document.getElementById("status");
  downloadLink = document.getElementById("downloadLink");
  formatSelect = document.getElementById("formatSelect");

  // Load the hardcoded video file
  videoEl.src = TASK_VIDEO_FILE;
  videoEl.load();

  // Disable direct user control before task starts
  videoEl.controls = false;
  videoEl.pause();
  videoEl.currentTime = 0;

  statusEl.textContent = "Task video loaded. Enable microphone to continue.";

  // Ask for microphone permission
  requestMicBtn.addEventListener("click", async () => {
    try {
      audioStream = await navigator.mediaDevices.getUserMedia({
        audio: {
          echoCancellation: true,
          noiseSuppression: true,
          autoGainControl: true
        },
        video: false
      });

      audioContext = new (window.AudioContext || window.webkitAudioContext)();
      statusEl.textContent = "Microphone enabled. Choose format and click 'Start task' when ready.";
      startTaskBtn.disabled = false;
      requestMicBtn.disabled = true;
    } catch (err) {
      console.error("Error getting microphone:", err);
      statusEl.textContent = "Microphone access denied or unavailable.";
    }
  });

  // Start task: go fullscreen, play video, record audio
  startTaskBtn.addEventListener("click", async () => {
    if (!audioStream) {
      statusEl.textContent = "Microphone not enabled.";
      return;
    }

    audioChunks = [];

    // Decide format
    const selectedFormat = formatSelect ? formatSelect.value : "default";

    // Base mimeType: default browser recording (WebM/OGG depending on browser)
    let mimeType = getSupportedMimeType(); // container/codec supported by MediaRecorder[web:24][web:27]

    // For WAV, we still record in the default container, then convert to WAV after
    // For MP3, we also record in default container, then convert to MP3 via lamejs

    try {
      mediaRecorder = new MediaRecorder(audioStream, { mimeType });
    } catch (e) {
      console.error("Failed to create MediaRecorder:", e);
      statusEl.textContent = "Recording not supported in this browser.";
      return;
    }

    mediaRecorder.ondataavailable = (event) => {
      if (event.data && event.data.size > 0) {
        audioChunks.push(event.data);
      }
    };

    mediaRecorder.onstop = async () => {
      statusEl.textContent = "Processing audio... please wait.";

      // Blob of recorded audio in default container (webm/ogg)
      const recordedBlob = new Blob(audioChunks, { type: mediaRecorder.mimeType });

      try {
        let finalBlob;
        let extension;

        if (selectedFormat === "mp3") {
          // Convert to MP3 using lamejs
          finalBlob = await convertToMp3(recordedBlob);
          extension = "mp3";
        } else if (selectedFormat === "wav") {
          // Convert to WAV
          finalBlob = await convertToWav(recordedBlob);
          extension = "wav";
        } else {
          // Default: keep original recording
          finalBlob = recordedBlob;
          // Use extension based on mime type
          if (mediaRecorder.mimeType.includes("ogg")) {
            extension = "ogg";
          } else if (mediaRecorder.mimeType.includes("webm")) {
            extension = "webm";
          } else {
            extension = "audio";
          }
        }

        const url = URL.createObjectURL(finalBlob);
        downloadLink.href = url;
        downloadLink.download = "participant_audio." + extension;
        downloadLink.style.display = "inline-block";

        statusEl.textContent = "Task complete! Download your recording below.";
      } catch (err) {
        console.error("Audio conversion failed:", err);
        statusEl.textContent = "Failed to process audio. Please try again.";
      }
    };

    // Request fullscreen on the video when starting the task[web:28][web:30][web:33]
    try {
      if (videoEl.requestFullscreen) {
        await videoEl.requestFullscreen();
      } else if (videoEl.webkitRequestFullscreen) {
        videoEl.webkitRequestFullscreen(); // Safari
      } else if (videoEl.msRequestFullscreen) {
        videoEl.msRequestFullscreen(); // IE11
      }
    } catch (e) {
      console.warn("Fullscreen request failed:", e);
      // Not fatal for recording
    }

    // Prevent user from manually controlling video (task should control playback)
    videoEl.controls = false;

    // Start recording and play video
    mediaRecorder.start();
    videoEl.currentTime = 0;
    videoEl.play();

    statusEl.textContent = "Task running • Recording audio...";
    startTaskBtn.disabled = true;
    downloadLink.style.display = "none";
  });

  // Stop recording when video ends
  videoEl.addEventListener("ended", () => {
    if (mediaRecorder && mediaRecorder.state === "recording") {
      mediaRecorder.stop();
    }
  });
});

// Helper: pick supported audio mime type for MediaRecorder[web:24][web:27]
function getSupportedMimeType() {
  const types = [
    "audio/webm;codecs=opus",
    "audio/webm",
    "audio/ogg;codecs=opus",
    "audio/ogg"
  ];
  for (const type of types) {
    if (MediaRecorder.isTypeSupported(type)) {
      return type;
    }
  }
  return "";
}

// Convert recorded blob to MP3 using lamejs (client-side)[web:11][web:12][web:31]
async function convertToMp3(audioBlob) {
  const arrayBuffer = await audioBlob.arrayBuffer();
  const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);

  // Use mono (first channel)
  const samples = audioBuffer.getChannelData(0);
  const sampleRate = audioBuffer.sampleRate;

  // Convert float samples [-1,1] to 16-bit signed PCM
  const int16Samples = new Int16Array(samples.length);
  for (let i = 0; i < samples.length; i++) {
    const s = Math.max(-1, Math.min(1, samples[i]));
    int16Samples[i] = s < 0 ? s * 0x8000 : s * 0x7FFF;
  }

  const mp3encoder = new lamejs.Mp3Encoder(1, sampleRate, 128); // mono, 128kbps
  const mp3Data = [];
  const sampleBlockSize = 1152;

  for (let i = 0; i < int16Samples.length; i += sampleBlockSize) {
    const sampleChunk = int16Samples.subarray(i, i + sampleBlockSize);
    const mp3buf = mp3encoder.encodeBuffer(sampleChunk);
    if (mp3buf.length > 0) {
      mp3Data.push(mp3buf);
    }
  }

  const mp3buf = mp3encoder.flush();
  if (mp3buf.length > 0) {
    mp3Data.push(mp3buf);
  }

  return new Blob(mp3Data, { type: "audio/mp3" });
}

// Convert recorded blob to WAV (simple PCM WAV header + data)[web:23][web:27][web:31]
async function convertToWav(audioBlob) {
  const arrayBuffer = await audioBlob.arrayBuffer();
  const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);

  const numChannels = 1; // force mono
  const sampleRate = audioBuffer.sampleRate;
  const samples = audioBuffer.getChannelData(0);
  const bufferLength = samples.length * 2; // 16-bit audio

  const wavBuffer = new ArrayBuffer(44 + bufferLength);
  const view = new DataView(wavBuffer);

  // Write WAV header
  writeWavHeader(view, sampleRate, numChannels, 16, bufferLength);

  // PCM samples
  let offset = 44;
  for (let i = 0; i < samples.length; i++, offset += 2) {
    const s = Math.max(-1, Math.min(1, samples[i]));
    view.setInt16(offset, s < 0 ? s * 0x8000 : s * 0x7FFF, true);
  }

  return new Blob([wavBuffer], { type: "audio/wav" });
}

// Helper to write WAV header[web:23][web:27][web:31]
function writeWavHeader(view, sampleRate, numChannels, bitsPerSample, dataLength) {
  const byteRate = (sampleRate * numChannels * bitsPerSample) >> 3;
  const blockAlign = (numChannels * bitsPerSample) >> 3;

  // RIFF identifier 'RIFF'
  view.setUint32(0, 0x52494646, false);
  // file length minus RIFF identifier length and file description length
  view.setUint32(4, 36 + dataLength, true);
  // RIFF type 'WAVE'
  view.setUint32(8, 0x57415645, false);
  // format chunk identifier 'fmt '
  view.setUint32(12, 0x666d7420, false);
  // format chunk length
  view.setUint32(16, 16, true);
  // sample format (raw)
  view.setUint16(20, 1, true);
  // channel count
  view.setUint16(22, numChannels, true);
  // sample rate
  view.setUint32(24, sampleRate, true);
  // byte rate (sample rate * block align)
  view.setUint32(28, byteRate, true);
  // block align (channel count * bytes per sample)
  view.setUint16(32, blockAlign, true);
  // bits per sample
  view.setUint16(34, bitsPerSample, true);
  // data chunk identifier 'data'
  view.setUint32(36, 0x64617461, false);
  // data chunk length
  view.setUint32(40, dataLength, true);
}
