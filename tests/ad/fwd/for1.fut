-- computes x^2*y^3
def pow y x = loop acc = 1 for i in [y, y*y] do
                acc * x * i
-- ==
-- entry: prim
-- input { 3 4 } output { 432 }
-- input { 9 3 } output { 6561 }

entry prim y x = pow y x

-- ==
<<<<<<< HEAD
-- entry: f_jvp 
-- input { 3 4 } output { 216 }
-- input { 9 3 } output { 4374 }
=======
-- entry: f_jvp
-- compiled input { 3 4 } output { 216 }
-- compiled input { 9 3 } output { 4374 }
>>>>>>> master

entry f_jvp y x = jvp (pow y) x 1
