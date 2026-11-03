-- The flattening attributes must survive the factoring of the redomap into a
-- map and a reduce; otherwise the map that comes out of the factoring is
-- multi-versioned, despite only_inner.
-- ==
-- input { [[1i64, 2i64], [3i64, 4i64]] [5i64, 6i64] }
-- auto output
-- structure gpu { If 0 }

def main [n] [m] (xss: [n][m]i64) (ys: [m]i64) =
  #[flattening(only_inner)]
  map (\xs -> i64.maximum (map (\x -> i64.sum (map (* x) ys)) xs)) xss
