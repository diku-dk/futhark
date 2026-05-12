-- It is ok to consuming non-free variables
-- ==
def consume (xs: *[]i64) : i64 = xs[0]
def f [n] (ns: [n]i64) (xs: [consume (iota 10)]f32) = xs[0]
