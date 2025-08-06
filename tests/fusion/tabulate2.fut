-- Indexing with iota elements, but the index is an inner dimension.
-- ==
-- input { 1i64 [[1,2,3],[4,5,6]] } output { [8,10,12] }
-- structure { Iota 0 }

def main [n] (j: i64) (xs: [][n]i32) =
  map (\i -> #[unsafe] xs[j, i] * 2) (iota n)
