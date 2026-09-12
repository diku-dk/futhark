-- A lambda that returns a global variable must still be rejected if it
-- escapes the enclosing named function, as the closure aliases travel
-- with the returned function value.
-- ==
-- error: aliases the free variable "global"

def global : []i64 = [1, 2, 3]

def main (_: i64) = \(_: i64) -> global
