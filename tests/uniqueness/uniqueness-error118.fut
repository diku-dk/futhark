-- A type parameter that comes from a call may occur only once in the result:
-- "dup" returns the result of one call twice, so the components are the same
-- array.
-- ==
-- error: consumed

def dup 'a 'b (f: a -> b) (x: a) : (b, b) =
  let r = f x
  in (r, r)

def main (x: i32) =
  let (a, b) = dup (replicate 3) x
  let a[0] = 0
  in (a, b)
