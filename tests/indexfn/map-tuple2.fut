def f xs ys : {[](i64,i64) | \_ -> true} =
  map (\(x,y) -> (x*y, x+y)) (zip xs ys)
