-- | Vectors in two dimensions.

import "/futlib/math"

module type vec2 = {
  type real
  type vec = {x: real, y: real}

  val +: vec -> vec -> vec
  val -: vec -> vec -> vec
  val dot: vec -> vec -> real
  val scale: real -> vec -> vec
  val norm: vec -> real
  val normalise: vec -> vec
}

module mk_vec2(real: real): vec2 with real = real.t = {
  type real = real.t

  type vec = {x: real, y: real}

  let (a: vec) + (b: vec) =
    real.({x=a.x+b.x, y=a.y+b.y})

  let (a: vec) - (b: vec) =
    real.({x=a.x-b.x, y=a.y-b.y})

  let dot (a: vec) (b: vec) =
    real.(a.x*b.x + a.y*b.y)

  let scale (s: real) ({x,y}: vec) =
    real.({x=x*s, y=y*s})

  let norm ({x,y}: vec): real =
    real.(sqrt (x*x + y*y))

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}
