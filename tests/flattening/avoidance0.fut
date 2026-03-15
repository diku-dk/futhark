-- ==
-- input { [3i64, 3i64, 3i64] [5i64,6i64,8i64] }
-- auto output
def main (xs: []i64) (ys: []i64) =
  unzip (map2 (\a b ->
                 let r1 = a - b
                 let r2 = a + b
                 let r3 = r1 * r1
                 let r4 = a * b + r3
                 in (r2, r4))
              xs
              ys)
