-- An existential produced through a higher-order function.
-- ==
-- input { [0, 1, 2] } output { 2i64 }

def main (xs: []i32) =
  let ys = xs |> filter (> 0)
  in length ys
