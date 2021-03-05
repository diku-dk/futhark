-- Testing for dynamic stencil index in 2D with tuples
-- ==
--
-- input { [[1,2,3,4,5],[5,4,3,2,1]] 
--		   [[1f32,2f32,3f32,4f32,5f32],[5f32,4f32,3f32,2f32,1f32]] 
--		   [[1i64,2i64,3i64,4i64,5i64],[5i64,4i64,3i64,2i64,1i64]] [1i64,-1i64]}
-- output { [[15f32,12f32,12f32,12f32,15f32],[15f32,12f32,12f32,12f32,15f32]]}

let main [n][m] (arri32 : [n][m]i32) (arrf32 : [n][m]f32) (arri64 : [n][m]i64) (ixs : [2]i64)
    : [n][m]f32 =
  let arr : [n][m](i32, f32, i64) = map3 zip3 arri32 arrf32 arri64
  let invar : [n][m]() = map (map (const ())) arr
  let f _ xs = (xs[0].0 + xs[1].0 + i32.f32 (xs[0].1 + xs[1].1), xs[0].2 + xs[1].2)
  let tmp = stencil_2d (zip ixs ixs) f invar arr
  in map (map (\(d,l) -> f32.i32 d + f32.i64 l)) tmp
