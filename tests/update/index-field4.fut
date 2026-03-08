-- Consecutive indexing in a single update path.
-- ==
-- input { 0i32 }
-- output { [[1i32, 0i32, 3i32], [4i32, 5i32, 6i32]] }

entry main (_: i32) : [2][3]i32 =
  let xss: *[][]i32 = [[1i32, 2i32, 3i32], [4i32, 5i32, 6i32]]
  let xss[0][1] = 0
  in xss
