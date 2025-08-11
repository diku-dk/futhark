-- An existing variable can be used as a shape declaration.
-- ==
-- input {
--   5i64
--   4i64
--   8i64
-- }
-- output {
--   [[6, 7, 8, 9, 10, 11, 12, 13],
--    [7, 8, 9, 10, 11, 12, 13, 14],
--    [8, 9, 10, 11, 12, 13, 14, 15],
--    [9, 10, 11, 12, 13, 14, 15, 16],
--    [10, 11, 12, 13, 14, 15, 16, 17]]
-- }

def main (n: i64) (m: i64) (k: i64) : [n][k]i32 =
  let a = replicate n (iota m)
  in map2 (\(i: i64) (r: [m]i64) : [k]i32 ->
             let x = reduce (+) 0 r
             in map i32.i64 (map (+ i) (map (+ x) (iota (k)))))
          (iota n)
          a
