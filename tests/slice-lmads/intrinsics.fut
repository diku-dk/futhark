let flat_index_3d [n] 'a (as: [n]a) (offset: i64) (n1: i64) (s1: i64) (n2: i64) (s2: i64) (n3: i64) (s3: i64) : [n1][n2][n3]a =
  intrinsics.flat_index_3d(as, offset, n1, s1, n2, s2, n3, s3) :> [n1][n2][n3]a

let flat_update_3d [n][k][l][p] 'a (as: *[n]a) (offset: i64) (s1: i64) (s2: i64) (s3: i64) (asss: [k][l][p]a) : *[n]a =
  intrinsics.flat_update_3d(as, offset, s1, s2, s3, asss)
