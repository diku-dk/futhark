-- tunnel.fut (WebGPU/canvas version)
--
-- Notes:
--  * We do NOT use Lys here. In the browser, JavaScript owns the window + timer
--    (requestAnimationFrame) and calls `entry main` every frame.
--  * We return one u32 per pixel, packed as RGBA bytes, because the JS canvas API
--    wants raw RGBA bytes. This avoids the matte/colour argb.colour dependency.
--  * We implement the 2D vector helpers locally (vadd/vsub/vdot/vnorm), so we
--    don't need to import the vspace library just for dot/norm.

type vec2 = {x: f32, y: f32}

def vadd (a: vec2) (b: vec2): vec2 = {x=a.x+b.x, y=a.y+b.y}
def vsub (a: vec2) (b: vec2): vec2 = {x=a.x-b.x, y=a.y-b.y}
def vdot (a: vec2) (b: vec2): f32 = a.x*b.x + a.y*b.y
def vnorm (a: vec2): f32 = f32.sqrt (vdot a a)

-- Fractional part.
def fract(x: f32): f32 =
  x - f32.i32(i32.f32(x))

def clamp(lower: f32, x: f32, upper: f32): f32 =
  if x < lower then lower
  else if x > upper then upper
  else x

def smoothstep(edge0: f32, edge1: f32, x: f32): f32 =
  let t = clamp(0f32, ((x-edge0) / (edge1-edge0)), 1.0f32)
  in t*t*(3f32 - 2f32*t)

def rand2(p: vec2): vec2 =
  let x = {x=127.1f32, y=311.7f32}
  let y = {x=269.5f32, y=183.3f32}
  in { x = fract(f32.sin(vdot p x) * 43758.5453f32)
     , y = fract(f32.sin(vdot p y) * 43758.5453f32) }

def rand1(p: vec2): f32 =
  let z = {x=419.2f32, y=371.9f32}
  in fract(f32.sin(vdot p z) * 833458.57832f32)

def sample(irregular: f32, cell: vec2, cellOffset: vec2, sharpness: f32, i: i32, j: i32): vec2 =
  let samplePos = {x=f32.i32 i, y=f32.i32 j}
  let u = rand2(vadd cell samplePos)
  let centre = {x=u.x * irregular, y=u.y * irregular}
  let centreDist = vnorm (vadd (vsub samplePos cellOffset) centre)
  let det = (1f32 - smoothstep(0f32, 1.414f32, centreDist)) ** sharpness
  let colour = rand1(vadd cell samplePos)
  in {x=colour * det, y=det}

def voronoise(xy: vec2, irregular: f32, smoothness: f32): f32 =
  let cell = {x=f32.i32(i32.f32 xy.x), y=f32.i32(i32.f32 xy.y)}
  let cellOffset = {x=fract xy.x, y=fract xy.y}
  let sharpness = 1f32 + 63f32 * ((1f32-smoothness) ** 4f32)
  let samples = loop samples = {x=0.0, y=0.0} for i in -2...2 do
    (loop samples for j in -2...2 do
      vadd samples (sample(irregular, cell, cellOffset, sharpness, i, j)))
  in samples.x / samples.y

def mod'(n: f32, d: f32): f32 =
  n - f32.i32(i32.f32(n/d)) * d

-- Pack (r,g,b,a) floats in [0..1] into a u32 pixel.
-- IMPORTANT: We pack so that the underlying bytes in memory become:
--   [R, G, B, A]
-- This matches what ImageData.data expects in JavaScript.
def rgba_u32 (r: f32) (g: f32) (b: f32) (a: f32): u32 =
  let cc (x: f32) = clamp(0f32, x, 1f32)
  let ri = u32.f32 (cc r * 255f32)
  let gi = u32.f32 (cc g * 255f32)
  let bi = u32.f32 (cc b * 255f32)
  let ai = u32.f32 (cc a * 255f32) -- opacity
  in (ai << 24) | (bi << 16) | (gi << 8) | ri

-- Compute one pixel of the tunnel at (x,y) for a given time.
-- Time makes the texture move, which creates the "flying through a tunnel" animation.
def tunnel(time: f32) (x: i32) (y: i32): u32 =
  let pt2 = {x=1.2f32 * f32.i32 x, y=1.2f32 * f32.i32 y}
  let rInv = 1.0f32 / vnorm pt2
  let pt3_unit = {x=pt2.x * rInv, y=pt2.y * rInv}
  let pt3 = vsub pt3_unit {x=rInv + 2.0f32 * mod'(time, 6000.0f32), y=0.0f32}
  let c1 = (0.659f32, 0.772f32, 1f32)
  let v = voronoise({x=5.0f32*pt3.x, y=5.0f32*pt3.y}, 1.0f32, 1.0f32) + 0.240f32*rInv
  in rgba_u32 (c1.0 * v) (c1.1 * v) (c1.2 * v) 1.0f32

-- Entry point called from JavaScript every frame:
-- JS computes time using requestAnimationFrame timestamps,
-- then calls this to produce an HxW image.
entry main (time: f32) (h: i32) (w: i32) =
  let hi = i64.i32 h
  let wi = i64.i32 w
  in tabulate_2d hi wi (\yy xx ->
       tunnel time
         (i32.i64 (xx - wi/2))
         (i32.i64 (yy - hi/2)))
