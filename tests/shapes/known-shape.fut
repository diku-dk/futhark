-- An existing variable can be used as a shape declaration.
-- ==
-- input {
--   5
--   4
--   8
-- }
-- output {
--   [[6, 7, 8, 9, 10, 11, 12, 13],
--    [7, 8, 9, 10, 11, 12, 13, 14],
--    [8, 9, 10, 11, 12, 13, 14, 15],
--    [9, 10, 11, 12, 13, 14, 15, 16],
--    [10, 11, 12, 13, 14, 15, 16, 17]]
-- }

let main (n: i32) (m: i32) (k: i32): [n][k]i32 =
  let a = replicate n (iota m) in
  map2 (\(i: i32) (r: [m]i32): [k]i32  ->
            let x = reduce (+) 0 r
            in map (+i) (map (+x) (iota(k))))
       (iota n) a
