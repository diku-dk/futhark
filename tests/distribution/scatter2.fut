-- Scattering where elements are themselves arrays - see #2035. This
-- one also has a map part.
-- ==
-- input { [[1,2,3],[4,5,6]] [1i64,0i64,-1i64] [[9,8,7],[6,5,4],[4,5,6]] }
-- output { [[8,7,6],[11,10,9]] }
-- structure gpu { SegMap 2 }

entry main (xss: *[][]i32) (is: []i64) (ys: [][]i32) =
  scatter xss is (map (map (+2)) ys)
