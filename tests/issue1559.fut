-- ==
-- random input { [100][10]i32 } auto output

def opt [n] [m] (arr: [m][n]i32) = reduce (map2 (\(x, i) (y, j) -> if x < y then (y, j) else (x, i))) (replicate n (i32.lowest, 0)) (map2 (\r i -> zip r (replicate n i)) arr (iota m))

def main arr = let (vs, is) = unzip (opt arr) in is
