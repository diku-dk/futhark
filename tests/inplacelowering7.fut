-- Do not move a consumption across a use of that array.
-- ==
-- random input { 10i64 [20]i32 } auto output
-- structure seq-mem { Update 2 }
-- structure gpu-mem { Update 1 }

def main (n: i64) (xs: *[]i32) =
  #[unsafe]
  let r = map i32.i64 (map (+2) (iota n))
  let x = xs[0]
  let ys = xs with [0:n] = r
  in (ys, x)
