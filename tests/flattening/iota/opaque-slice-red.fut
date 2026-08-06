-- ==
-- input { [1i64,2i64] [0i64,1i64] }
-- output { [0i64,1i64] }
-- input { [1i64,5i64] [0i64,3i64] }
-- output { [0i64,7i64] }
-- input { [1i64,2i64] [0i64,3i64] }
-- error: .
-- input { [1i64,-2i64] [0i64,1i64] }
-- error: .
-- structure gpu { Iota 0 /Apply/segiota 1 }

def main ns is =
  #[incremental_flattening(only_inner)]
  map2 (\n (i: i64) -> i64.sum (opaque (iota n))[i:]) ns is
