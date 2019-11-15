-- This example presents difficulty for me right now, but also has a
-- large potential for improvement later on.
--
-- we could turn it into:
--
-- let []i32 bettermain ([]i32 xs, [#n]i32 ys, [#n]i32 zs, [#n]i32 is, [#n]i32 js) =
--   map (\i32 (i32 y, i32 z, i32 i, i32 j) ->
--          xs[i*z + j]
--       , zip(ys,zs,is,js))

let main [n][m] (xs: [m]i32, ys: [n]i32, zs: [n]i32, is: [n]i32, js: [n]i32): []i32 =
  map  (\(y: i32, z: i32, i: i32, j: i32): i32  ->
         unsafe
         let tmp = unflatten y z xs
         in tmp[i,j]
      ) (zip4 ys zs is js)
