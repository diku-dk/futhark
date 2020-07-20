type~ sized_state [n] = { xs: [n][n]i32, ys: []i32 }
type~ state = sized_state []

let state v : state = {xs = [[v,2],[3,4]], ys = [1,2,3]}

let size [n] (_: sized_state [n]) = n

let f v (arg: state) =
  size (arg :> sized_state [v])

-- ==
-- input { 2 } output { 2 }

let main v = f v (state v)
