-- Testing static index input
-- ==
-- 
-- input { [1f32,2f32,3f32,4f32,5f32] } output { [8f32,11f32,15f32,19f32,22f32] }

let main (ys :[]f32) =
  let f _ xs = xs[0] + xs[1] + xs[2] + xs[3] + xs[4]
  in stencil_1d [2i64,-1,0,1,-2] f (map (const ()) ys) ys
