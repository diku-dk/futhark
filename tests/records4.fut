-- Record pattern in function parameter.
-- ==
-- input { 2 } output { 1 3 }

def f {a = a: i32, b = c: i32} = (a, c)

-- And with a little fancier ascription.
type t = {c: i32, d: i32}
def g ({c, d}: t) = f {a = c, b = d}

def main (x: i32) =
  g {c = x - 1, d = x + 1}
