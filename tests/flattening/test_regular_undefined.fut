-- Triggers the current undefined case in onMapInputArr.
-- ==
-- input { [1i64, 2i64, 3i64] }
-- auto output

def main (xs: []i64) =
  map (\x -> map (+1) [x, x*x]) xs
