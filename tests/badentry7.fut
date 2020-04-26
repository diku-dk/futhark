-- Don't throw away module qualifiers when looking at opaque types for
-- entry points.
-- ==
-- warning: Entry point parameter

module m0 = {
  type state = {f: i32}
}

module m1 = {
  type state = {f: [1]f32}
}

entry g (p0: m0.state) (p1: m1.state) =
  r32 p0.f + p1.f[0]
