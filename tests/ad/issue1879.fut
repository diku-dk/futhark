-- ==
-- tags { autodiff }
-- entry: main_ad
-- input { [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32]] }
-- output { [[1.0f32, 1.0f32, 1.0f32], [1.0f32, 1.0f32, 1.0f32]] }
-- input { [[1.0f32, 2.0f32, 3.0f32], [7.0f32, 8.0f32, 9.0f32]] }
-- output {  [[1.0f32, 1.0f32, 1.0f32], [0.0f32, 0.0f32, 0.0f32]] }

def f [n] (xs: [n][3]f32) =
  let p x = if x[0] < 5 then true else false
  let (res, sizes) = partition p xs
  in res |> flatten |> f32.sum

entry main_ad xs =
  vjp f xs 1.0
