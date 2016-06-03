-- ==
-- input { 8 }
-- output { [1, 2, 3, 4, 5, 6, 7, 0] }

fun [int] main(int i) =
  let a = iota(i) in
  rotate(0, -1, a)
