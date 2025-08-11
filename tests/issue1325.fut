-- ==
-- random input { [100]bool [100]i64 [10][10]i32 [10][10]i32 } auto output
-- compiled random input { [1000]bool [1000]i64 [10][10]i32 [10][10]i32 } auto output
-- structure gpu-mem { If/False/Replicate 0 If/True/Replicate 0 }

def main [n] [k] (bs: []bool) (is: []i64) (xs: [n][k]i32) (ys: [n][k]i32) =
  #[unsafe]
  map2 (\b i ->
          let j = i % n
          in if b then xs[j] else ys[j])
       bs
       is
