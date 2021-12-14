-- Intragroup relaxation.
-- ==
-- compiled no_python random input { 100 [100][256]f32 } auto output

def relax (xs: []f32) =
  map2 (+) xs (map2 (+) (rotate (-1) xs) (rotate 1 xs))

def main (steps: i32) (xss: [][]f32) =
  map (iterate steps relax) xss
