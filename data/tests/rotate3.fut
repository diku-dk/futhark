-- ==
-- input { 8 4 }
-- output { [[1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32],
--           [1i32, 2i32, 3i32, 0i32], [1i32, 2i32, 3i32, 0i32]] }

fun [[int]] main(int n, int m) =
  let a = replicate(n, iota(m))
  in rotate(1, -1, a)
