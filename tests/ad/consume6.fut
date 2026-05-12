-- ==
-- tags { autodiff }

def test [n] (xs: [n]i32) =
  let xs' = copy xs
  in loop xs'' = xs'
     for i < n do
       let foo = xs'' with [i] = 1
       let m = map (\x -> x) foo
       in foo with [i] = m[i]

-- ==
-- entry: prim
-- input { [1,2,3,4,5] } output { [1,1,1,1,1] }
entry prim [n] (xs: [n]i32) = test xs

-- ==
-- entry: f_vjp
-- input { [1,2,3,4,5] } output { [0,0,0,0,0] }
entry f_vjp [n] (xs: [n]i32) = vjp test xs (replicate n 1)
