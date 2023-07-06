-- ==
-- random input { [2000]i32 }
-- auto output
-- structure gpu-mem { Alloc 0 }

def main (xs: *[]i32) = take 1000 (opaque (map (+1) xs))
