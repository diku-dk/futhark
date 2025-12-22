def add (x, y) (z,a) : {(i64,i64) | \_ -> true} = (x+z, y+a)

def f xs ys : {[](i64,i64) | \_ -> true} =
  map (\(x, y) ->
    let xy = (x,y)
    let v = (2,3)
    in add xy v) (zip xs ys)
