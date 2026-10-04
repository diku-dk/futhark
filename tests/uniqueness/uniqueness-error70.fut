-- A function may return a lambda that returns a global array, but then
-- what applying the lambda returns cannot be consumed.
-- ==
-- error: "f", which is not consumable

def global : []i64 = [1, 2, 3]

def f (_: i64) = \(_: i64) -> global

def main (n: i64) = f n n with [0] = 0
