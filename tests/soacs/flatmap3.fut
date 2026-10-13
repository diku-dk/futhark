-- flatmap nested inside an enclosing map, exercising the segmented flattening
-- path. For each row we return reductions over the flatmap's data, metadata and
-- value result.
-- ==
-- input { [[2i64, 3i64], [1i64, 4i64]] [[10i32, 20i32], [30i32, 40i32]] }
-- output { [5i64, 5i64] [2i64, 2i64] [2i64, 1i64] [84i32, 196i32] [60i32, 140i32] }

def main (kss: [][]i64) (xss: [][]i32) =
  map2 (\ks xs ->
          let (shape, flag, offset, r, c) =
            flatmap (\(k, x) -> (map (\i -> x + i32.i64 i) (iota k), x * 2)) (zip ks xs)
          in ( i64.sum shape
             , i64.sum (map i64.bool flag)
             , i64.sum offset
             , i32.sum r
             , i32.sum c
             ))
       kss
       xss
  |> unzip5
