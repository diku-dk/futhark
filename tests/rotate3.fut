-- ==
-- input { 8 4 }
-- output { [[1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32]] }

fun main(n: int, m: int): [][]int =
  let a = replicate n (iota m)
  in rotate@1 1 a
