-- Testing for dynamic stencil index in 2D
-- ==
--
-- input { [[1,2,3,4,5],[5,4,3,2,1]] [[1,2,3,4,5],[1,2,3,4,5]] [[1,2,3,4,5],[5,4,3,2,1]] }
-- output { [[13,12,16,19],[13,12,16,19]]}

let main [n][m] (arri32 : [n][m]i32) (arrf32 : [n][m]f32) (arri64 : [n][m]i64)
    : [n][m]f32 =
  let arr : [n][m](i32, f32, i64) = map3 zip3 arri32 arrf32 arri64
  let invar : [n][m]() = map (map (const ())) arr
  let f _ xs = (xs[0].0 + xs[1].0 + i32.f32 (xs[0].1 + xs[1].1), xs[0].2 + xs[1].2)
  let tmp = stencil_2d [(1,1),(-1,-1)] f invar arr
  in map (map (\(d,l) -> f32.i32 d + f32.i64 l)) tmp
