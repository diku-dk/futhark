-- This example presents difficulty for me right now, but also has a
-- large potential for improvement later on.
--
-- we could turn it into:
--
-- fun []int bettermain ([]int xs, [n]int ys, [n]int zs, [n]int is, [n]int js) =
--   map (fn int (int y, int z, int i, int j) =>
--          xs[i*z + j]
--       , zip(ys,zs,is,js))

fun main (xs: []int, ys: [n]int, zs: [n]int, is: [n]int, js: [n]int): []int =
  map (fn (y: int, z: int, i: int, j: int): int  =>
         unsafe
         let tmp = reshape( (y,z) , xs ) in
         tmp[i,j]
      , zip(ys,zs,is,js))
