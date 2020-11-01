
let main [n] (xs: [n]i32): ([n]i32, [n]i32) =
  unzip <| scan (\(x0, x1) (y0, y1) -> (y0, y1)) (0, 0) <| map (\x -> (-x, x)) xs
