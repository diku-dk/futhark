-- A slice is hoisted out of a map only if a hoisted statement depends on it.
-- Here the reduction that uses the slice also depends on a variant value, so
-- it stays in the map, and so must the slice - otherwise the slice would need
-- a pointless fallback branch.
-- ==
-- tags { no_webgpu }
-- input { [[1f32,2f32],[3f32,4f32]] 1i64 }
-- output { [0f32, 7f32] }
-- structure { /Match 0 }

def main [n] [m] (xss: [n][m]f32) (i: i64) =
  map (\j -> let b = f32.i64 j in f32.sum (map (* b) xss[i])) (iota n)
