-- ==
-- input { 8 4 }
-- output { [[1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32]] }

let main(n: i32, m: i32): [][]i32 =
  let a = replicate n (iota m)
  in rotate@1 1 a
