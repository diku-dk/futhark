import "/futlib/math"

module type vec3 = {
  type real
  type vec = {x: real, y: real, z: real}

  val +: vec -> vec -> vec
  val -: vec -> vec -> vec
  val dot: vec -> vec -> real
  val scale: real -> vec -> vec
  val norm: vec -> real
  val normalise: vec -> vec
}

module mk_vec3(real: real): vec3 with real = real.t = {
  type real = real.t

  type vec = {x: real, y: real, z: real}

  let (a: vec) + (b: vec) =
    real.({x=a.x+b.x, y=a.y+b.y, z=a.z+b.z})

  let (a: vec) - (b: vec) =
    real.({x=a.x-b.x, y=a.y-b.y, z=a.z-b.z})

  let dot (a: vec) (b: vec) =
    real.(a.x*b.x + a.y*b.y + a.z*b.z)

  let scale (s: real) ({x,y,z}: vec) =
    real.({x=x*s, y=y*s, z=z*s})

  let norm ({x,y,z}: vec): real =
    real.(sqrt (x*x + y*y + z*z))

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}
