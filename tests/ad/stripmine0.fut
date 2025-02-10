def pow y x =
  #[stripmine(3)]
  loop acc = 1
  for _i < y do
    acc * x

-- ==
-- entry: f_jvp f_vjp
-- input { 3 4 } output { 48 }
-- input { 9 3 } output { 59049 }
-- no_python compiled input { 1000000 1 } output { 1000000 }
entry f_jvp y x = jvp (pow y) x 1
entry f_vjp y x = vjp (pow y) x 1
