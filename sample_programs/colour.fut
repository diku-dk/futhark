-- | Colour manipulation library.
--
-- Adapted from the [Gloss](https://hackage.haskell.org/package/gloss)
-- library by Ben Lippmeier.

-- | A colour that can be converted back and forth between an RGBA
-- representation.  Not very useful by itself, but using just this
-- interface one can generate a lot of other useful functions via the
-- colourspace parametric module.
module type colour = {
  type colour

  -- | Construct a colour from R, G, B and A channels, each of which
  -- must be a floating-point number between 0.0 and 1.0.  The
  -- concrete representation need not be able to handle the full
  -- precision of each channel.  Thus, `from_rgba` and `to_rgba` need
  -- not be inverse of each other (but should be close).
  val from_rgba: f32 -> f32 -> f32 -> f32 -> colour

  -- | Convert a colour to four R, G, B and A channels, each of which
  -- is a floating-point number between 0.0 and 1.0.
  val to_rgba: colour -> (f32, f32, f32, f32)
}

-- | A colour representation that encodes the four RGBA channels as a
-- byte each in a 32-bit word, using the order A-R-G-B.
module argb_colour: colour with colour = u32 = {
  -- ARGB storage.
  type colour = u32

  def clamp_channel (x: f32): f32 =
    if x < 0f32 then 0f32 else if x > 1f32 then 1f32 else x

  def from_rgba (r: f32) (g: f32) (b: f32) (a: f32): colour =
    ((u32.f32 (clamp_channel a * 255) << 24) |
     (u32.f32 (clamp_channel r * 255) << 16) |
     (u32.f32 (clamp_channel g * 255) << 8)  |
     (u32.f32 (clamp_channel b * 255)))

  def to_rgba (x: colour): (f32,f32,f32,f32) =
    (f32.u32 ((x>>16) & 0xFF) / 255,
     f32.u32 ((x>>8) & 0xFF) / 255,
     f32.u32 ((x>>0) & 0xFF) / 255,
     f32.u32 ((x>>24) & 0xFF) / 255)
}

-- | A colour representation and a host of useful functions and constants.
module type colourspace = {
  include colour

  -- | Add RGB components of a color component-wise, then normalise
  -- them to the highest resulting one. The alpha components are
  -- averaged.
  val add: colour -> colour -> colour

  -- | Add RGBA components of a color component-wise, capping them at
  -- the maximum.
  val add_linear: colour -> colour -> colour

  val mult: colour -> colour -> colour
  val scale: colour -> f32 -> colour
  val mix: f32 -> colour -> f32 -> colour -> colour

  -- | Brighten 20%.
  val bright: colour -> colour
  -- | Dim 20%.
  val dim: colour -> colour
  -- | 20% lighter.
  val light: colour -> colour
  -- | 20% darker.
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

  -- | Grayness from 0-1.
  val gray: f32 -> colour
}

-- | Given a colour representation, construct a colourspace with all
-- the handy functions and constants.
module colourspace(C: colour): colourspace with colour = C.colour = {
  open C

  def from_rgb_normalised (r: f32) (g: f32) (b: f32): colour =
    let m = f32.max r (f32.max g b)
    in from_rgba (r / m) (g / m) (b / m) 1f32

  -- Normalise a color to the value of its largest RGB component.
  def normalised_colour (r: f32) (g: f32) (b: f32) (a: f32): colour =
    let m = f32.max r (f32.max g b)
    in from_rgba (r / m) (g / m) (b / m) a

  def add (x: colour) (y: colour): colour =
    let (r1,g1,b1,a1) = to_rgba x
    let (r2,g2,b2,a2) = to_rgba y
    in normalised_colour
       (f32.max r1 r2)
       (f32.max g1 g2)
       (f32.max b1 b2)
       ((a1+a2)/2f32)

  def add_linear (x: colour) (y: colour): colour =
    let (r1,g1,b1,a1) = to_rgba x
    let (r2,g2,b2,a2) = to_rgba y
    in from_rgba (r1+r2) (g1+g2) (b1+b2) (a1+a2)

  def mult (x: colour) (y: colour): colour =
    let (r1,g1,b1,a1) = to_rgba x
    let (r2,g2,b2,a2) = to_rgba y
    in from_rgba (r1*r2) (g1*g2) (b1*b2) (a1*a2)

  def scale (x: colour) (s: f32): colour =
    let (r,g,b,a) = to_rgba x
    in from_rgba (r*s) (g*s) (b*s) (a*s)

  def mix (m1: f32) (c1: colour) (m2: f32) (c2: colour): colour =
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


  def bright (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r * 1.2f32) (g * 1.2f32) (b * 1.2f32) a

  def dim (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r * 0.8f32) (g * 0.8f32) (b * 0.8f32) a

  def light (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r + 0.2f32) (g + 0.2f32) (b + 0.2f32) a

  def dark (c: colour): colour =
    let (r,g,b,a) = to_rgba c
    in from_rgba (r - 0.2f32) (g - 0.2f32) (b - 0.2f32) a

  -- Basic colours
  def black: colour = from_rgba 0f32 0f32 0f32 1f32
  def red: colour = from_rgba 1f32 0f32 0f32 1f32
  def green: colour = from_rgba 0f32 1f32 0f32 1f32
  def blue: colour = from_rgba 0f32 0f32 1f32 1f32
  def white: colour = from_rgba 1f32 1f32 1f32 1f32
  def brown: colour = from_rgba 0.49f32 0.19f32 0.11f32 1f32

  -- Derived colours
  def yellow: colour = add red green
  def orange: colour = add yellow red
  def magenta: colour = add red blue
  def violet: colour = add magenta blue

  def gray (d: f32): colour = from_rgba d d d 1f32
}

-- | An ARGB colour space - simply `colourspace`@term applied to
-- `argb_colour`@term.
module argb: colourspace with colour = argb_colour.colour = colourspace argb_colour
