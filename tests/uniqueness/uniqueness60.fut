-- If consumption is on bounded var, no problem
-- ==
-- warning: ^$
def consume (xs: *[]i64) : i64 = xs[0]
def f (n: i64) = iota (consume (iota n))
