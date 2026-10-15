-- The result of applying a lambda that returns a global array cannot be
-- consumed.
-- ==
-- error: "global", which is not consumable

def global : []i64 = [1, 2, 3]

def main (n: i64) = (\(_: i64) -> global) n with [0] = 0
