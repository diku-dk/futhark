-- Generate arrays in a nested flatmap and then do something with them.
-- ==
-- input { [[2i64,3i64],[1i64,4i64]] [[10i32,20i32],[30i32,40i32]] }
-- output { [[80, 80], [190, 190]] }

def main (kss: [][]i64) (xss: [][]i32) =
  map2 (\ks xs ->
          let (_, _, _, r) =
            flatmap (\(k, x) -> replicate k (replicate 2 x)) (zip ks xs)
          in map (\x -> foldl (+) 0 x) (transpose r))
       kss
       xss
