-- Anonymous array element type misused.
-- ==
-- error: Cannot apply "reverse" to "x"

def reverse [n] [m] 't (a: [m][n]t) = a[::-1]

def main (x: []i32) = reverse x
