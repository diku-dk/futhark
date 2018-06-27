-- | A module type for vectors of any (static) dimension.
--
-- The parametric modules `mk_vec2`@term and `mk_vec3`@term are
-- intended for small vectors that can be represented efficiently as
-- records.  For larger-dimensional vectors, use `mk_vec`@term, which
-- uses an array as the internal representation.

module type vec = {
  type real

  -- | A vector type.  Semantically a sequence of `real`s.
  type vec

  val +: vec -> vec -> vec
  val -: vec -> vec -> vec

  -- | Inner product.
  val dot: vec -> vec -> real

  -- | Squared norm.
  val quadrance: vec -> real

  val scale: real -> vec -> vec
  val norm: vec -> real

  -- | Transform to unit vector.
  val normalise: vec -> vec
}

module type vec2 = {
  type real

  include vec with real = real with vec = {x: real, y: real}
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

  let quadrance v = dot v v

  let scale (s: real) ({x,y}: vec) =
    real.({x=x*s, y=y*s})

  let norm = quadrance >-> real.sqrt

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}

module type vec3 = {
  type real

  include vec with real = real with vec = {x: real, y: real, z: real}

  -- | Cross product.
  val cross: vec -> vec -> vec
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

  let cross ({x=ax,y=ay,z=az}: vec)
            ({x=bx,y=by,z=bz}: vec): vec =
    real.({x=ay*bz-az*by, y=az*bx-ax*bz, z=ax*by-ay*bx})

  let quadrance v = dot v v

  let scale (s: real) ({x,y,z}: vec) =
    real.({x=x*s, y=y*s, z=z*s})

  let norm = quadrance >-> real.sqrt

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}

-- | Given a scalar representation and a module containing a single
-- value indicating a dimensionality, produce a module that implements
-- `vec`@mtype.
module mk_vec (real: real) (N : { val d : i32 }): vec with real = real.t
                                                      with vec = [N.d]real.t = {
  type real = real.t
  type vec = [N.d]real.t

  let (+) = map2 (real.+)

  let (-) = map2 (real.+)

  let dot (a: vec) (b: vec) = real.sum (map2 (real.*) a b)

  let scale (s: real) = map (s real.*)

  let quadrance v = dot v v

  let norm = quadrance >-> real.sqrt

  let normalise (v: vec): vec =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}
