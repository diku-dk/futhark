-- ==
-- entry: index
-- compiled random input { [100]i32 0i64 10i64 -20i64 10i64 20i64 10i64 1i64 }
-- error: out of bounds
-- compiled random input { [27]i32 0i64 10i64 1i64 10i64 1i64 10i64 1i64 }
-- error: out of bounds
-- random input { [30]i32 0i64 10i64 1i64 10i64 1i64 10i64 1i64 }
-- compiled random input { [27]i32 2i64 10i64 1i64 10i64 1i64 10i64 1i64 }
-- error: out of bounds
-- compiled random input { [1001]i32 1i64 10i64 100i64 10i64 10i64 10i64 1i64 }
-- compiled random input { [1000]i32 1i64 10i64 100i64 10i64 10i64 10i64 1i64 }
-- error: out of bounds

-- ==
-- entry: index_2d
-- compiled random input { [91]i32 0i64 5i64 20i64 11i64 1i64 }

-- ==
-- entry: update
-- compiled random input { [100]i32 0i64 -20i64 20i64 1i64 [10][10][10]i32 }
-- error: out of bounds
-- compiled random input { [27]i32 0i64 1i64 1i64 1i64 [10][10][10]i32 }
-- error: out of bounds
-- random input { [30]i32 0i64 1i64 1i64 1i64 [1][1][1]i32 }
-- compiled random input { [27]i32 2i64 1i64 1i64 1i64 [10][10][10]i32 }
-- error: out of bounds
-- compiled random input { [1001]i32 1i64 100i64 10i64 1i64 [10][10][10]i32 }
-- compiled random input { [1000]i32 1i64 100i64 10i64 1i64 [10][10][10]i32 }
-- error: out of bounds

import "intrinsics"

entry index [n] (xs: [n]i32) (offset: i64) (n1: i64) (s1: i64) (n2: i64) (s2: i64) (n3: i64) (s3: i64) : [][][]i32 =
  flat_index_3d xs offset n1 s1 n2 s2 n3 s3

entry index_2d [n] (xs: [n]i32) (offset: i64) (n1: i64) (s1: i64) (n2: i64) (s2: i64) : [][]i32 =
  flat_index_2d xs offset n1 s1 n2 s2

entry update [n] (xs: *[n]i32) (offset: i64) (s1: i64) (s2: i64) (s3: i64) (vs: [][][]i32) : [n]i32 =
  flat_update_3d xs offset s1 s2 s3 vs
