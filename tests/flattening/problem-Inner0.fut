-- ==
-- input { [3i64, 7i64, 10i64, 2i64, 20i64] }
-- auto output
-- input { [3i64,55] }
-- auto output
-- input { [3i64, 7i64, 10i64] }
-- auto output
def main (xs: []i64) =
  map (\x ->
         let res = opaque (iota x)
         let mes = map (+5) res
         -- some paralell operations
         in mes[1])
      xs