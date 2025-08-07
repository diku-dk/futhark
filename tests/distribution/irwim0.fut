-- ==
-- random input { 5i64 [3][4][5]i32 [3][4][5]i32 } auto output

def main k xs ys =
  let op (x, a) (y, b) = (x + y, a * b)
  let (xs, as) =
    unzip (map unzip (map (reduce (map2 op) (replicate k (0, 1)))
                          (map2 (map2 zip) xs ys)))
  in xs
