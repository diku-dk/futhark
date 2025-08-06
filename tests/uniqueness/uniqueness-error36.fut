-- Ensure that we cannot cheat uniqueness typing with higher-order
-- functions.
-- ==
-- error: consuming

def apply 'a 'b (f: a -> b) (x: a) = (f x, f x)

def consume (xs: *[]i32) = 0

def main (arr: *[]i32) =
  apply consume arr
