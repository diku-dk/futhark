
let main [n] (xs: [n]i32): ([n]i32, [n]i32, [n]i32, [n]i32) =
  let (as, bs) = unzip <| map (\x -> (-x, x)) xs
  let (cs, ds) = unzip <| scan (\(x0, x1) (y0, y1) -> (y0, y1)) (0, 0) <| zip as bs
  in (as, bs, cs, ds)
