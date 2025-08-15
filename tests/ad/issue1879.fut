-- ==
-- tags { autodiff }
-- entry: main_ad
-- input { [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]] }
-- output { [[1.0, 1.0, 1.0], [1.0, 1.0, 1.0]] }
-- input { [[1.0, 2.0, 3.0], [7.0, 8.0, 9.0]] }
-- output {  [[1.0, 1.0, 1.0], [0.0, 0.0, 0.0]] }

def f [n] (xs: [n][3]f64) =
  let p x = if x[0] < 5 then true else false
  let (res, sizes) = partition p xs
  in res |> flatten |> f64.sum

entry main_ad xs =
  vjp f xs 1.0
