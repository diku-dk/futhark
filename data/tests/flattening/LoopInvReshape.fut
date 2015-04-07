// This example presents difficulty for me right now, but also has a
// large potential for improvement later on.
//
// we could turn it into:
//
// fun [int] bettermain ([int] xs, [int,n] ys, [int,n] zs, [int,n] is, [int,n] js) =
//   map (fn int (int y, int z, int i, int j) =>
//          xs[i*z + j]
//       , zip(ys,zs,is,js))

fun [int] main ([int] xs, [int,n] ys, [int,n] zs, [int,n] is, [int,n] js) =
  map (fn int (int y, int z, int i, int j) =>
         let tmp = reshape( (y,z) , xs ) in
         tmp[i,j]
      , zip(ys,zs,is,js))
