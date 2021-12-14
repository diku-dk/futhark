-- It should be allowed to have a shape parameter that is only used in
-- negative position in the parameter types.
-- ==
-- input {} output { 3i64 }

def f [n] (_g: i32 -> [n]i32) : i64 = n

def main = f (replicate 3)
