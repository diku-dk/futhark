#[inline]
def flat_index_2d [n] 'a (as: [n]a) (offset: i64) (n1: i64) (s1: i64) (n2: i64) (s2: i64) : [n1][n2]a =
  intrinsics.flat_index_2d as offset n1 s1 n2 s2 :> [n1][n2]a

#[inline]
def flat_update_2d [n][k][l] 'a (as: *[n]a) (offset: i64) (s1: i64) (s2: i64) (asss: [k][l]a) : *[n]a =
  intrinsics.flat_update_2d as offset s1 s2 asss

#[inline]
def flat_index_3d [n] 'a (as: [n]a) (offset: i64) (n1: i64) (s1: i64) (n2: i64) (s2: i64) (n3: i64) (s3: i64) : [n1][n2][n3]a =
  intrinsics.flat_index_3d as offset n1 s1 n2 s2 n3 s3 :> [n1][n2][n3]a

#[inline]
def flat_update_3d [n][k][l][p] 'a (as: *[n]a) (offset: i64) (s1: i64) (s2: i64) (s3: i64) (asss: [k][l][p]a) : *[n]a =
  intrinsics.flat_update_3d as offset s1 s2 s3 asss

#[inline]
def flat_index_4d [n] 'a (as: [n]a) (offset: i64) (n1: i64) (s1: i64) (n2: i64) (s2: i64) (n3: i64) (s3: i64) (n4: i64) (s4: i64) : [n1][n2][n3][n4]a =
  intrinsics.flat_index_4d as offset n1 s1 n2 s2 n3 s3 n4 s4 :> [n1][n2][n3][n4]a

#[inline]
def flat_update_4d [n][k][l][p][q] 'a (as: *[n]a) (offset: i64) (s1: i64) (s2: i64) (s3: i64) (s4: i64) (asss: [k][l][p][q]a) : *[n]a =
  intrinsics.flat_update_4d as offset s1 s2 s3 s4 asss
