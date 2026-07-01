-- ==
-- input { [3i64, 7i64, 10i64, 1i64, 20i64] }
-- auto output
def main (xs: []i64) =
  map (\x ->
         let g = x * 5 * 100
         let y = g * 55
         let r2 = y * 2
         let t = y + 100 + r2
         let ys = iota x
         let z = t * 2
         in z * g + i64.sum ys)
      xs
