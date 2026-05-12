-- Can we recognise an invented size when it occurs later?
-- ==
-- input { 2i64 }
-- output { [[0i64, 0i64, 0i64, 0i64], [1i64, 1i64, 1i64, 1i64], [2i64, 2i64, 2i64, 2i64], [3i64, 3i64, 3i64, 3i64]] }

def main n =
  let is = iota (n + n)
  in map (\x -> replicate (n + n) x) is
