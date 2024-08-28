def pow y x = loop acc = 1 for i < y do
                acc * x
-- ==
-- entry: prim
-- compiled input { 3 4 } output { 64 }
-- compiled input { 9 3 } output { 19683 }

entry prim y x = pow y x

-- ==
-- entry: f_jvp
-- compiled input { 3 4 } output { 48 }
-- compiled input { 9 3 } output { 59049 }

entry f_jvp y x = jvp (pow y) x 1
