let main (ys :[]f32) =
  let f _ xs = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])/5
  in stencil_1d [-2,-1,0,1,2] f (map (const ()) ys) ys
