-- Hessian-vector product of the sum of squared array-valued prefixes.
-- H*v is twice the suffix sum of the prefix sums of v, componentwise.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[1f32, 2], [3f32, 4], [5f32, 6]]
--         [[1f32, 2], [3f32, 4], [5f32, 6]] }
-- output { [[28f32, 40], [26f32, 36], [18f32, 24]] }
-- input { [[2f32, 3]] [[4f32, 5]] }
-- output { [[8f32, 10]] }
-- input { empty([0][2]f32) empty([0][2]f32) }
-- output { empty([0][2]f32) }

entry main [n] (xs: [n][2]f32) (direction: [n][2]f32) =
  let objective ys =
    scan (\x y -> [x[0] + y[0], x[1] + y[1]]) [0, 0] ys
    |> map (\p -> p[0] * p[0] + p[1] * p[1])
    |> reduce (+) 0
  in jvp (\ys -> vjp objective ys 1) xs direction
