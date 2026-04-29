-- A test for preserving result order when flattening a distributed inner map.
-- ==
-- input { [[1i32, 2i32], [3i32, 4i32]]  3i32}
-- auto output

def main [n] [m] (xss: [n][m]i32) (k: i32) =
  map (\xs ->
         let a = map (+ 1) xs
         let b = map (+ k) a
         in (b, a))
      xss
  |> unzip
