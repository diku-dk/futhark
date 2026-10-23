-- Simplifying a reduction of known size must not make the unique result
-- alias the neutral element.
-- ==
-- entry: empty
-- input { [3,4] } output { [3,4] }

-- ==
-- entry: single
-- input { [[1,2]] [3,4] } output { [3,4] }

entry empty (ne: [2]i32) : [2]i32 =
  reduce (map2 (+)) ne (replicate 0 [1i32, 2])

entry single (xss: [1][2]i32) (ne: [2]i32) : [2]i32 =
  reduce (\x _ -> x) ne xss
