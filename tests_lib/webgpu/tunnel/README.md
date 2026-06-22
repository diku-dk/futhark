# Tunnel (WebGPU + timer-driven animation)

This example renders a procedural "tunnel" effect written in **Futhark**, compiled to **WebGPU**, and displayed in a browser `<canvas>`.

The important part (for this repository) is that the animation is **driven by a timer**: JavaScript uses `requestAnimationFrame` (the browser’s animation clock) to repeatedly call the Futhark entry point with an increasing `time` parameter.

## What you should see

A tunnel-like pattern with a center "vanishing point" and a moving texture.  
Changing the **Time scale** slider makes the animation move faster/slower (0 freezes it).

## Files

* `tunnel.fut`
  The Futhark program. It exports:

  * `entry main(time: f32, h: i32, w: i32) -> [h][w]u32`

  Each `u32` is one pixel, packed so that its memory bytes are `[R, G, B, A]` (so JS can copy it directly into `ImageData`).

* `index.html`
  UI layout (canvas + status panel + slider) and script includes.

* `style.css`
  Page styling.

* `main.js`
  The "glue" code:

  * initializes the generated WebGPU runtime
  * runs a timer-driven render loop
  * copies pixels to the HTML canvas
  * shows some status/log output

* `Makefile`
  Builds the WebGPU library output and runs a simple local server.

* Output from `futhark webgpu --library`, e.g. `tunnel.js`, `tunnel.wasm`, etc.
  These files are generated and should not be committed.

## How the generated JavaScript works

Running:

```sh
futhark webgpu --library -o tunnel tunnel.fut
```

produces `tunnel.js` (and companion files). That generated file provides two main things:

1. `Module()`
   This is the Emscripten/WASM loader. Calling `await Module()` loads and initializes the WebAssembly module.

2. `FutharkModule`
   This is a wrapper that knows how to interact with the compiled Futhark program using WebGPU.

   * `await fut.init(m)` creates/initializes the WebGPU device and internal runtime state.
   * `fut.entry.main(...)` calls the Futhark entry point `main`.

When you call an entry point like `fut.entry.main(time, h, w)`, you get back an object representing a Futhark array result. To use it in JS, we do:

* `await buf.values()` to copy the result back into a typed array in JS memory
* `buf.free()` to free the underlying WebGPU/WASM resources

Freeing is important in an animation loop; otherwise you would leak a new output buffer every frame.

## The animation is "timer-driven"

The browser calls our `frame(now)` callback via:

```js
requestAnimationFrame(frame)
```

`now` is a high-resolution timestamp. We compute how much real time passed since the last frame (`dt`) and advance the simulation time:

```js
dt = (now - lastNow) / 1000.0
t  += dt * speed
```

Then we render using that `t`:

```js
await fut.entry.main(t, height, width)
```

So the animation speed is based on **elapsed real time** (not "one step per frame").
The slider just scales how much time we advance per real second; the timer (`requestAnimationFrame`) is still the source of "time passing".

## What the Futhark code does (high level)

* For each pixel `(x, y)` it computes a procedural value using:

  * inverse radius from the center (`rInv`) to create a tunnel/depth feel
  * a Voronoi-like noise function ("voronoise") to add texture
* The `time` parameter shifts the sampling position, making the texture move each frame.
* The output is packed to RGBA bytes so JS can blit it directly into a canvas.

There are more comments in `tunnel.fut` if you want to read the details.

## Notes on SharedArrayBuffer

The status box prints whether `SharedArrayBuffer` is available. If it says "unavailable", that’s normal when serving without special headers (COOP/COEP). This example still works fine without it; it just affects some possible fast paths (e.g., multithreading in some setups).
