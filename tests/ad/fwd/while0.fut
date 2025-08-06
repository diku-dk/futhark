def pow y x =
  let (_, res) =
    loop (i, acc) = (0, 1)
    while i < y do
      (i + 1, acc * x)
  in res

-- ==
-- entry: prim
-- input { 3 4 } output { 64 }
-- input { 9 3 } output { 19683 }

entry prim y x = pow y x

-- ==
-- entry: f_jvp
-- input { 3 4 } output { 48 }
-- input { 9 3 } output { 59049 }

entry f_jvp y x = jvp (pow y) x 1
