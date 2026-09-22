-- Hessian-vector product of the sum of squared array-valued prefixes.
-- H*v is twice the suffix sum of the prefix sums of v, componentwise.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[1f64, 2], [3f64, 4], [5f64, 6]]
--         [[1f64, 2], [3f64, 4], [5f64, 6]] }
-- output { [[28f64, 40], [26f64, 36], [18f64, 24]] }
-- input { [[2f64, 3]] [[4f64, 5]] }
-- output { [[8f64, 10]] }
-- input { empty([0][2]f64) empty([0][2]f64) }
-- output { empty([0][2]f64) }

entry main [n] (xs: [n][2]f64) (direction: [n][2]f64) =
  let objective ys =
    scan (\x y -> [x[0] + y[0], x[1] + y[1]]) [0, 0] ys
    |> map (\p -> p[0] * p[0] + p[1] * p[1])
    |> reduce (+) 0
  in jvp (\ys -> vjp objective ys 1) xs direction
