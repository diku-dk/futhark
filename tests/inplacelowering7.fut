-- Do not move a consumption across a use of that array.
-- ==
-- random input { 10i64 [20]i32 } auto output
-- structure seq-mem { Update 2 Alloc 1 }
-- structure gpu-mem { Update 1 Alloc 1 }

def main (n: i64) (xs: *[]i32) =
  #[unsafe]
  -- The source write of one thread of the map must not overlap the destination uses of the other iterations in the map. We also need to check that the entire thing being written does not overlap previous uses of the destination. These two checks should not interfere with each other?
  let r = map i32.i64 (map (+2) (iota n))
  let x = xs[0]
  let ys = xs with [0:n] = r
  in (ys, x)
