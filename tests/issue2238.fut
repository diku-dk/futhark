-- ==
-- input { 2.0 }

type sum = #a f64 | #b f64

entry main (x: f64) : (i32, f64) =
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
