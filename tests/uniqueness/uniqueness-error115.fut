-- A recursive application consumes the arguments passed for consuming
-- parameters, also when the function has no declared return type.
-- ==
-- error: Using variable "xs", but this was consumed

def f (xs: *[]i32) (n: i32) = if n == 0 then xs[0] else f xs (n - 1) + xs[0]
