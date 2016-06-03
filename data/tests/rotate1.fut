-- ==
-- input { 8 }
-- output { [7, 0, 1, 2, 3, 4, 5, 6] }

fun [int] main(int i) =
  let a = iota(i) in
  rotate(0, 1, a)
