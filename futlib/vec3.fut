import "/futlib/math"

module type vec3 = {
  type real
  type vec = (real,real,real)

  val +: vec -> vec -> vec
  val -: vec -> vec -> vec
  val dot: vec -> vec -> real
  val scale: real -> vec -> vec
  val norm: vec -> real
  val normalise: vec -> vec
}

module mk_vec3(real: real): vec3 with real = real.t = {
  type real = real.t

  type vec = (real,real,real)

  let ((x1,y1,z1): vec) + ((x2,y2,z2): vec) =
    (x1 real.+ x2, y1 real.+ y2, z1 real.+ z2)

  let ((x1,y1,z1): vec) - ((x2,y2,z2): vec) =
    (x1 real.- x2, y1 real.- y2, z1 real.- z2)

  let dot ((x1,y1,z1): vec) ((x2,y2,z2): vec) =
    (x1 real.* x2) real.+ (y1 real.* y2) real.+ (z1 real.* z2)

  let scale (s: real) ((x,y,z): vec) =
    (x real.* s, y real.* s, z real.* s)

  let norm ((x,y,z): vec): real =
    real.sqrt (x real.* x real.+ y real.* y real.+ z real.* z)

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.from_i32 1 real./ l) v
}
