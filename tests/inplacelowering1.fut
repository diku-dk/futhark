-- Careful with the in-place forwarding here.
-- ==

let main (xs: *[20]i32) (ys: [20]i32) =
  let ys' = ys[0:10]
  let x = xs[4]
  let xs[0:10] = ys'
  in (x, xs)
