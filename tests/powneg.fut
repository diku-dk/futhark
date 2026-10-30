-- Do not crash during constant folding if encountering a negative
-- exponent.
-- ==
-- input { true } error:

def main b = if b then 2 ** -1 else 0

-- The operator from the i32 module must also reject a negative
-- exponent.
-- ==
-- entry: qualified
-- input { 2 -1 } error:

entry qualified (x: i32) (y: i32) = (i32.**) x y
