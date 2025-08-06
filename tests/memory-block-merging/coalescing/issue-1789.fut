-- ==
-- input { [1,2,3,4,5,6,7,8,9] }
-- output { [4,5,6,7,8,9,7,8,9] }
-- structure gpu-mem { Alloc 1 }

entry main [n] (xs: *[n]i32) : [n]i32 =
  let i = n / 3
  in xs with [:2 * i] = copy xs[i:]
