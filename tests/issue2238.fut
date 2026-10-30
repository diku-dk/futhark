-- ==
-- tags { autodiff }
-- input { 2.0f32 }

type sum = #a f32 | #b f32

entry main (x: f32) : (i32, f32) =
  let g =
    vjp (\(x: sum) ->
           match x
           case #a x' -> x' ** 2
           case #b x' -> x' ** 3)
        (#b x)
        1
  in match g
     case #a x' -> (0, x')
     case #b x' -> (1, x')
