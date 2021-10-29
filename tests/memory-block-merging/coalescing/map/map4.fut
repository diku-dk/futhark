-- ==
-- input { [7i64, 8i64, 9i64] }
-- output { [[0i64, 8i64, 18i64],
--           [1i64, 9i64, 19i64],
--           [2i64, 10i64, 20i64]] }
-- structure gpu { Alloc 1 }
-- structure cpu { Alloc 2 }

let main [n] (xs: [n]i64): [n][n]i64 =
  map (\j ->
    loop xs' = copy xs for i < n do
    let xs'[i] = xs'[i] * i + j
    in xs'
  ) (iota n)
