-- Testing static index input
-- ==
-- 
-- input { [1f32,2,3,4,5] } output { [8f32,11,15,19,22] }

let main (ys :[]f32) =
  let f _ xs = xs[0] + xs[1] + xs[2] + xs[3] + xs[4]
  in stencil_1d [2i64,-1,0,1,-2] f (map (const ()) ys) ys
