-- A size-dependent function (its result size depends on the argument value)
-- cannot be mapped.
-- ==
-- error: Cannot apply

def g (k: i64) : [k]i64 = iota k

def main (n: i64) = map g (iota n)
