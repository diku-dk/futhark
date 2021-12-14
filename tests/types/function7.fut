-- Array dimensions in function type may refer to previous named parameters.

def f (g: (n: i64) -> [n]i32) = g 0
