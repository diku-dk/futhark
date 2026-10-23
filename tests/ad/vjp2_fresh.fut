-- The primal result of vjp2 is fresh, even when the function returns a
-- free variable.
-- ==
-- input { [1f32,2f32] 10f32 }
-- output { [42f32,11f32] [10f32,11f32] }

entry main (xs: [2]f32) (k: f32) =
  let zs = map (\i -> f32.i64 i + k) (iota 2)
  let (p, _) = vjp2 (\(_: [2]f32) -> zs) xs zs
  let p[0] = 42
  in (p, zs)
