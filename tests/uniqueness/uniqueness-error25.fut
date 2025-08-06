-- ==
-- error: "t", but this was consumed

def f (t: ([]i32, *[]i32)) : i32 =
  let (a, b) = t
  let b[0] = 1337
  in a[0]

def main (b: *[]i32) : i32 =
  let a = b
  -- Should fail, because 'a' and 'b' are aliased, yet the 'b' part of
  -- the tuple is consumed.
  in f ((a, b))
