-- Generate arrays in a nested flatmap and then do something with them, while
-- also using the value result.
-- ==
-- input { [[2i64,3i64],[1i64,4i64]] [[10i32,20i32],[30i32,40i32]] }
-- output { [[80i32, 80i32], [190i32, 190i32]] [60i32, 140i32] }

def main (kss: [][]i64) (xss: [][]i32) =
  map2 (\ks xs ->
          let (_, _, _, r, c) =
            flatmap (\(k, x) -> (replicate k (replicate 2 x), x * 2)) (zip ks xs)
          in (map (\x -> foldl (+) 0 x) (transpose r), i32.sum c))
       kss
       xss
  |> unzip
