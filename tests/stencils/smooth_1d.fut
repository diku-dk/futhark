-- ==

let smooth (xs: []f32) =
  let f () xs = (xs[0] + xs[1] + xs[2] + xs[3] + xs[4])/5
  in stencil_1d [-2,-1,0,1,2] f (map (const ()) xs) xs

let main = iterate 10 smooth
