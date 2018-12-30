-- CSE once messed this one up after kernel extraction, because the
-- same input array is used in two reduce kernels, and reduce-kernels
-- may consume some input arrays (specifically, the scratch input
-- array).
--
-- ==
-- structure distributed { SegRed 2 }

let max(a: f32) (b: f32): f32 = if(a < b) then b else a

let exactYhat(xs: []f32, x: f32): f32 =
  let ups = map (+x) xs
  let lo = reduce max (0.0) ups
  in lo + ups[0]

let main (xs: []f32, mux: f32, eps: f32): f32 =
  let g = exactYhat(xs, mux + eps)
  let h = exactYhat(xs, mux - eps)
  in g + h
