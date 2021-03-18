-- Testing dynamic input
--
-- ==
-- input { [2i64,-1i64,0i64,1i64,-2i64] [1,2,3,4,5] } 
-- output { [8,11,15,19,22] }

let main (ixs : [5]i64) (ys :[]i32) =
  let f _ xs = xs[0] + xs[1] + xs[2] + xs[3] + xs[4]
  in stencil_1d ixs f (map (const ()) ys) ys