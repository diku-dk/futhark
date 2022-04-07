-- ==
-- random input { [100][10]i32 }
-- auto output
-- structure gpu-mem { Alloc 3 }

def psum = scan (+) 0

def main (xss: [][]i32) =
  #[incremental_flattening(only_intra)]
  map (psum >-> psum >-> psum)
      xss
