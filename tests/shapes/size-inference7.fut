-- Does it work to have a definition that only has a size parameter?
-- ==
-- input { 3i64 }
-- output { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64]
--          [2, 2, 2, 2, 2, 2] }

def iiota [n] : [n]i64 = iota n

def main x = unzip (zip iiota (replicate (2 * x) 2i32))
