-- Same value is used in two updates.  Currently does not optimise as
-- well as it ought.
-- ==
-- random input { 10i64 [20]i32 [30]i32 } auto output
-- structure seq-mem { Update 2 }
-- structure gpu-mem { Update 1 }

def main (n: i64) (xs: *[]i32) (ys: *[]i32) =
  #[unsafe]
  let a = map i32.i64 (map (+2) (iota n))
  in (xs with [0:n] = a, ys with [0:n] = a)
