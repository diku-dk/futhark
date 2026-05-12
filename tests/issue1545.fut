-- ==
-- input { [[1,2,3],[4,5,6]] }
-- output { [[11i32, 0i32, 0i32], [14i32, 0i32, 0i32]] }

def main [n] (xss: [][n]i32) =
  #[sequential_inner]
  map (\xs ->
         let xs' = loop xs = copy xs for _i < 10 do map (+ 1) xs
         let ys = replicate n 0
         let ys[0] = xs'[0]
         in ys)
      xss
