-- It is bad to give an argument with a binding that is used in size
-- but it is accepted
-- ==
-- warning: with binding

def f [n] (ns: *[n]i64) = iota (let m = n + 2 in m * m)
