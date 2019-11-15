-- Concat-scatter fusion in a more complicated session.
-- ==
-- input { [0,5] }
-- output { [3, 5, 1, 1, 1, 3, 5, 1, 1, 1]
--          [4, 6, 2, 2, 2, 4, 6, 2, 2, 2] }
-- structure { Concat 0 Scatter 1 }

let main [n] (xs: [n]i32) =
  let dest = replicate 10 (1,2)
  let (is0, vs0, is1, vs1) = unzip4 (map (\x -> (x,(3,4),x+1,(5,6))) xs)
  let m = n + n
  in unzip (scatter dest (concat is0 is1 :> [m]i32) (concat vs0 vs1 :> [m](i32,i32)))
