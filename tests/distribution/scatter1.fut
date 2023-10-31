-- Scattering where elements are themselves arrays - see #2035.
-- ==
-- input { [[1,2,3],[4,5,6]] [1i64,0i64,-1i64] [[9,8,7],[6,5,4],[9,8,7]] }
-- output { [[6,5,4],[9,8,7]] }

entry main (xss: *[][]i32) (is: []i64) (ys: [][]i32) =
  scatter xss is ys
