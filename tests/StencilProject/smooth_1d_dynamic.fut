let smooth (ixs : [5]i64) (xs: []f32) =
  let f () xs = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])/5
  in stencil_1d ixs f (map (const ()) xs) xs

let main (ixs : [5]i64) = smooth ixs [1,2,3,4,5,6,7,8]
