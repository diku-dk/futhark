-- ==
-- input { [[0i32, 1i32, 2i32], [3i32, 4i32, 5i32]] }
-- auto output

def main (xss: [][]i32) =
  #[incremental_flattening(only_intra)]
  map (\xs ->
         let ys = map (+ 1) xs |> opaque
         let s = reduce (+) 0 ys
         in map (+ s) xs)
      xss