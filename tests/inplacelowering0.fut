-- ==
-- random input { 10i64 [20]i32 } auto output
-- structure cpu { Update 1 }
-- structure gpu { Update 0 }

let main (n: i64) (xs: *[]i32) =
  #[unsafe]
  xs with [0:n] = map i32.i64 (map (+2) (iota n))
