-- An existential produced through a higher-order function.
-- ==
-- input { [0, 1, 2] } output { 2 }

let main (xs: []i32) =
  let ys = xs |> filter (>0)
  in length ys
