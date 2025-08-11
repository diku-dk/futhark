-- The usual problems with identity mapping.

def main (xss: [][]i32) (ys: []i32) =
  let (as, bs) = unzip2 (map2 (\xs y -> (y, i32.sum (map (+ y) xs))) xss ys)
  in (i32.sum as, bs)
