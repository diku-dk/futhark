-- ==
-- random input { [1001]i32 }
-- auto output
-- structure gpu-mem { Copy 0 }
def main [n] (xs: [n]i32): i32 =
  let xs' = if n > 10
            then xs[1:5]
            else rotate 3 xs
  in reduce (+) 0 xs'
