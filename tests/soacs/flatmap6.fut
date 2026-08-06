-- A flatmap nested in a map, whose element type is an array, where we also test
-- the forms of the metadata.
-- ==
-- input { [[2i64,3i64],[1i64,4i64]] [[10i32,20i32],[30i32,40i32]] }
-- output { [5i64, 5i64] [2i64, 2i64] [2i64, 1i64] [160i32, 380i32] }

def main (kss: [][]i64) (xss: [][]i32) =
  map2 (\ks xs ->
          let (shape, flag, offset, r) =
            flatmap (\k x -> replicate k (replicate 2 x)) ks xs
          in ( i64.sum shape
             , i64.sum (map i64.bool flag)
             , i64.sum offset
             , i32.sum (map (\row -> row[0] + row[1]) r)
             ))
       kss
       xss
  |> unzip4
