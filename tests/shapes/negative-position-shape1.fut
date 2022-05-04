-- It should not be allowed to have a shape parameter that is only
-- used in negative position in the parameter types, but only if that
-- size is unambiguous.
-- ==
-- error: Ambiguous size.*instantiated size parameter of "f"

def f [n] (g: [n]i64 -> i64) : i64 = n

def main = f (\xs -> xs[0])
