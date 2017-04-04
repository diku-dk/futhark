-- Use of a shape annotation in a let-binding.  From issue #239.  For
-- now, banned.
--
-- ==
-- error: .*shape declaration.*

default (f32)

let main(a: [rows][cols]f32): [rows]f32 =
  let means: [rows]f32 = map (\a -> (reduce (+) 0.0 a) / f32(cols)) a in
  means
