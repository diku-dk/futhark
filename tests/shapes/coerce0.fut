type~ sized_state [n] = {xs: [n][n]i64, ys: []i32}
type~ state = sized_state []

def state v : state = {xs = [[v, 2], [3, 4]], ys = [1, 2, 3]}

def size [n] (_: sized_state [n]) = n

def f v (arg: state) =
  size (arg :> sized_state [v])

-- ==
-- input { 2i64 } output { 2i64 }

def main v = f v (state v)
