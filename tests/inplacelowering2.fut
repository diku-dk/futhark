-- ==
-- input { [[0,0,0], [0,0,0]] }
-- output { [[2,3,4], [0,0,0]] }
-- structure seq-mem { Update 1 }
-- structure gpu-mem { Update 0 }

def main [n] (xs: *[][n]i32) =
  #[unsafe]
  xs with [0] = map i32.i64 (map (+2) (iota n))
