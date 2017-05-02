-- Colour manipulation library.
--
-- Adapted from the gloss library by Ben Lippmeier:
-- https://hackage.haskell.org/package/gloss

import "/futlib/math"

module type colour = {
  type colour

  val from_rgba: f32 -> f32 -> f32 -> f32 -> colour
  val to_rgba: colour -> (f32, f32, f32, f32)
}

module argb_colour: colour with colour = i32 = {
  -- ARGB storage.
  type colour = i32

  let clamp_channel (x: f32): f32 =
    if x < 0f32 then 0f32 else if x > 1f32 then 1f32 else x

  let from_rgba (r: f32) (g: f32) (b: f32) (a: f32): colour =
    ((i32 (clamp_channel a * 255f32) << 24) |
     (i32 (clamp_channel r * 255f32) << 16) |
     (i32 (clamp_channel g * 255f32) << 8)  |
     (i32 (clamp_channel b * 255f32)))

  let to_rgba (x: colour): (f32,f32,f32,f32) =
    (f32 ((x>>16) & 0xFF) / 255f32,
     f32 ((x>>8) & 0xFF) / 255f32,
     f32 ((x>>0) & 0xFF) / 255f32,
     f32 ((x>>24) & 0xFF) / 255f32)
}

module type colourspace = {
  include colour

  val add: colour -> colour -> colour
  val mix: f32 -> colour -> f32 -> colour -> colour

  -- Brighten 20%
  val bright: colour -> colour
  -- Dim 20%
  val dim: colour -> colour
  -- 20% lighter
  val light: colour -> colour
  -- 20% darker
  val dark: colour -> colour

  -- Basic colours
  val black: colour
  val red: colour
  val green: colour
  val blue: colour
  val white: colour
  val brown: colour

  -- Derived colours
  val yellow: colour
  val orange: colour
  val magenta: colour
  val violet: colour

  -- Grayness from 0-1.
  val gray: f32 -> colour
}

module colourspace(C: colour): colourspace with colour = C.colour = {
  open C

  let max_channel (x: f32) (y: f32): f32 =
    if x < y then y else x

  let from_rgb_normalised (r: f32) (g: f32) (b: f32): colour =
    let m = max_channel r (max_channel g b)
    in from_rgba (r / m) (g / m) (b / m) 1f32

  -- Normalise a color to the value of its largest RGB component.
  let normalised_colour (r: f32) (g: f32) (b: f32) (a: f32): colour =
    let m = max_channel r (max_channel g b)
    in from_rgba (r / m) (g / m) (b / m) a

  let add (x: colour) (y: colour): colour =
    let (r1,g1,b1,a1) = to_rgba x
    let (r2,g2,b2,a2) = to_rgba y
    in normalised_colour
       (max_channel r1 r2)
       (max_channel g1 g2)
       (max_channel b1 b2)
       ((a1+a2)/2f32)

  let mix (m1: f32) (c1: colour) (m2: f32) (c2: colour): colour =
    let (r1,g1,b1,a1) = to_rgba c1
    let (r2,g2,b2,a2) = to_rgba c2

    let m12 = m1 + m2
    let m1' = m1 / m12
    let m2' = m2 / m12

    let r1s = r1 * r1
    let r2s = r2 * r2

    let g1s = g1 * g1
    let g2s = g2 * g2

    let b1s = b1 * b1
    let b2s = b2 * b2

    in from_rgba (f32.sqrt (m1' * r1s + m2' * r2s))
                 (f32.sqrt (m1' * g1s + m2' * g2s))
                 (f32.sqrt (m1' * b1s + m2' * b2s))
                 ((m1 * a1 + m2 * a2) / m12)


  let bright (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r * 1.2f32) (g * 1.2f32) (b * 1.2f32) a

  let dim (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r * 0.8f32) (g * 0.8f32) (b * 0.8f32) a

  let light (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r + 0.2f32) (g + 0.2f32) (b + 0.2f32) a

  let dark (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r - 0.2f32) (g - 0.2f32) (b - 0.2f32) a

  -- Basic colours
  let black: colour = from_rgba 0f32 0f32 0f32 1f32
  let red: colour = from_rgba 1f32 0f32 0f32 1f32
  let green: colour = from_rgba 0f32 1f32 0f32 1f32
  let blue: colour = from_rgba 0f32 0f32 1f32 1f32
  let white: colour = from_rgba 1f32 1f32 1f32 1f32
  let brown: colour = from_rgba 0.49f32 0.19f32 0.11f32 1f32

  -- Derived colours
  let yellow: colour = add red green
  let orange: colour = add yellow red
  let magenta: colour = add red blue
  let violet: colour = add magenta blue

  let gray (d: f32): colour = from_rgba d d d 1f32
}

module argb = colourspace(argb_colour)
