-- ==
-- input { [[1,2,3], [4,5,6], [7,8,9]] 1i64 }
-- output { [[2,3,4], [5,6,7], [8,9,10]] }
-- structure gpu { Alloc 0 }
-- structure cpu { Alloc 0 }

let main [n] (xsss: *[n][n][n]i32) (i: i64) =
  let xss = map (map (+ 1)) xsss[i]
  let xsss[i] = xss
  in xsss
