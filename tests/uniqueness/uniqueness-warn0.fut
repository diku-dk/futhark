-- It is bad to give consuming argument that is used in size
-- but it is accepted
-- ==
-- warning: consuming argument is used in the return type,
def consume (xs: *[]i64): i64 = xs[0]
def f [n] (ns: *[n]i64) = iota (consume ns)