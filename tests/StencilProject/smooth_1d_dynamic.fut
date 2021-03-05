let main (ixs : [5]i64) (ys :[]f32) =
  let f _ xs = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])/5
  in stencil_1d ixs f (map (const ()) ys) ys
