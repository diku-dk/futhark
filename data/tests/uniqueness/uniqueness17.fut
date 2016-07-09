-- CSE once messed this one up after kernel extraction, because the
-- same input array is used in two reduce kernels, and reduce-kernels
-- may consume some input arrays (specifically, the scratch input
-- array).
--
-- ==
-- structure distributed { Kernel 4 }

default(f32)

fun f32 main ([]f32 xs, f32 mux, f32 eps) =
  let g = exactYhat(xs, mux + eps)
  let h = exactYhat(xs, mux - eps)
  in g + h

fun f32 exactYhat([]f32 xs, f32 x) =
  let ups = map(+x, xs)
  let lo = reduce(max, 0.0, ups)
  in lo + ups[0]

fun f32 max(f32 a, f32 b) = if(a < b) then b else a
