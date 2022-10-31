-- ==
-- input { [[ 0, 1, 2, 3], [ 4, 5, 6, 7]] }
-- output { [[0i32, 1i32, 5i32, 15i32], [4i32, 17i32, 45i32, 95i32]] }
-- random input { [100][10]i32 }
-- auto output
-- structure gpu-mem { Alloc 3 }

def psum = scan (+) 0

def main (xss: [][]i32) =
  #[incremental_flattening(only_intra)]
  map (psum >-> psum >-> psum)
      xss
