async function run() {
  const statusEl = document.getElementById("status");
  const statusBox = statusEl.parentElement; // .statusBox
  const canvas = document.getElementById("canvas");
  const ctx = canvas.getContext("2d");

  const speedEl = document.getElementById("speed");
  const speedValEl = document.getElementById("speedVal");

  const LOG_MAX_LINES = 40;
  const logLines = [];

  function log(line) {
    logLines.push(line);
    if (logLines.length > LOG_MAX_LINES) {
      logLines.splice(0, logLines.length - LOG_MAX_LINES); // buffer
    }
    statusEl.textContent = logLines.join("\n");
    statusBox.scrollTop = statusBox.scrollHeight;
  }

  // environment checks
  log("Checking WebGPU support...");
  const hasWebGPU = typeof navigator !== "undefined" && !!navigator.gpu;
  log(hasWebGPU ? "WebGPU available." : "WebGPU NOT available.");

  const hasSAB = typeof SharedArrayBuffer !== "undefined";
  log(hasSAB ? "SharedArrayBuffer available." : "SharedArrayBuffer NOT available.");

  if (!hasWebGPU) {
    log("");
    log("Cannot run: WebGPU is missing. Try a newer Chrome/Edge, or enable WebGPU.");
    return;
  }

  log("Initializing Emscripten module...");
  const m = await Module();

  log("Creating Futhark context (WebGPU device)...");
  const fut = new FutharkModule(); 
  await fut.init(m);
  log("Futhark context ready.");

  const width = canvas.width;
  const height = canvas.height;
  const imgData = ctx.createImageData(width, height);

  // Slider UI
  function currentSpeed() {
    return Number(speedEl.value);
  }
  function updateSpeedLabel() {
    speedValEl.textContent = currentSpeed().toFixed(1);
  }
  speedEl.addEventListener("input", updateSpeedLabel);
  updateSpeedLabel();

  // animation loop starts here
  let t = 0.0;               // simulation time passed to Futhark
  let lastNow = performance.now();

  // Simple perf stats (updated ~once/sec)
  let frames = 0;
  let accComputeMs = 0;
  let lastStatsNow = lastNow;

  async function frame(now) {
    const dt = (now - lastNow) / 1000.0;
    lastNow = now;

    // advance time based on slider
    t += dt * currentSpeed();

    // render one frame
    const tCompute0 = performance.now();
    const res = await fut.entry.main(t, height, width);
    const buf = Array.isArray(res) ? res[0] : res;

    const vals = await buf.values();
    const bytes = new Uint8Array(vals.buffer, vals.byteOffset, vals.length * 4);

    imgData.data.set(bytes);
    ctx.putImageData(imgData, 0, 0);

    buf.free();
    const tCompute1 = performance.now();

    // Stats
    frames += 1;
    accComputeMs += (tCompute1 - tCompute0);

    if (now - lastStatsNow >= 1000) {
      const fps = frames * 1000 / (now - lastStatsNow);
      const avgCompute = accComputeMs / frames;

      // rewrite the last stats section without spamming too hard:
      log(`FPS: ${fps.toFixed(1)} | avg frame compute: ${avgCompute.toFixed(2)} ms | speed: ${currentSpeed().toFixed(1)}x`);

      frames = 0;
      accComputeMs = 0;
      lastStatsNow = now;
    }

    requestAnimationFrame(frame);
  }

  requestAnimationFrame(frame);
}

window.addEventListener("load", run);
