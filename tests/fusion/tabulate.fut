-- Indexing with iota elements is turned into proper mapping, thus
-- permitting fusion.
-- ==
-- input { [1,2,3] } output { [1,4,9] }
-- structure { Screma 1 }

let main [n] (xs: [n]i32) =
  let ys = map (\i -> xs[i]) (iota n)
  in map (\i -> ys[i] * xs[i]) (iota n)
