-- ==
-- tags { autodiff }

def pow y x =
  loop acc = 1
  for _i < y do
    acc * x

-- ==
-- entry: prim
-- input { 3 4 } output { 64 }
-- input { 9 3 } output { 19683 }
entry prim y x = pow y x

-- ==
-- entry: f_jvp f_vjp
-- input { 3 4 } output { 48 }
-- input { 9 3 } output { 59049 }
entry f_jvp y x = jvp (pow y) x 1
entry f_vjp y x = vjp (pow y) x 1
