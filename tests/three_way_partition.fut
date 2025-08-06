-- A manually implemented partitioning, hardcoded for 3 equivalence classes.
--
-- ==
-- input { [1f32, 2f32, 3f32, 4f32, 5f32, 6f32, 7f32, 8f32, 9f32]
--         [0i64, 1i64, 2i64, 3i64, 0i64, 1i64, 2i64, 3i64, 0i64] }
-- output { 3i64 2i64 2i64 [1f32, 5f32, 9f32, 2f32, 6f32, 3f32, 7f32] }

def main [n] (vs: [n]f32) (classes: [n]i64) : (i64, i64, i64, []f32) =
  let flags =
    map (\c ->
           if c == 0
           then (1, 0, 0)
           else if c == 1
           then (0, 1, 0)
           else if c == 2
           then (0, 0, 1)
           else (0, 0, 0))
        classes
  let is0 = scan (\(a0, b0, c0) (a1, b1, c1) -> (a0 + a1, b0 + b1, c0 + c1)) (0, 0, 0) flags
  let (size_0, size_1, size_2) = is0[n - 1]
  let filter_size = size_0 + size_1 + size_2
  let is1 =
    map2 (\(ai, bi, ci) c ->
            if c == 0
            then ai - 1
            else if c == 1
            then size_0 + bi - 1
            else if c == 2
            then size_0 + size_1 + ci - 1
            else -1)
         is0
         classes
  in ( size_0
     , size_1
     , size_2
     , spread filter_size 0 is1 vs
     )
