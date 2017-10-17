-- The initial values for merge parameters must have the right size.
-- ==
-- input { [1,2,3] 3 }
-- output { [4,4,4] }
-- input { [1] 3 }
-- error:

let main (xs: []i32) (n: i32) =
  loop (ys: [n]i32) = xs for i < 3 do
    replicate n (ys[0]+1)
