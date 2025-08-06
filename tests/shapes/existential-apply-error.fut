-- An existential size in an apply function returning an unlifted type is not fine.
-- ==
-- error: existential

def apply 'a 'b (f: a -> b) (x: a) : b =
  f x

def main (n: i32) = apply iota n
