import "/futlib/math"

module type vec2 = {
  type real
  type vec = (real,real)

  val +: vec -> vec -> vec
  val -: vec -> vec -> vec
  val dot: vec -> vec -> real
  val scale: real -> vec -> vec
  val norm: vec -> real
  val normalise: vec -> vec
}

module mk_vec2(real: real): vec2 with real = real.t = {
  type real = real.t

  type vec = (real,real)

  let ((x1,y1): vec) + ((x2,y2): vec) =
    (x1 real.+ x2, y1 real.+ y2)

  let ((x1,y1): vec) - ((x2,y2): vec) =
    (x1 real.- x2, y1 real.- y2)

  let dot ((x1,y1): vec) ((x2,y2): vec) =
    (x1 real.* x2) real.+ (y1 real.* y2)

  let scale (s: real) ((x,y): vec) =
    (x real.* s, y real.* s)

  let norm ((x,y): vec): real =
    real.sqrt (x real.* x real.+ y real.* y)

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}
