-- It is not OK to define the same field twice.
-- ==
-- error: previously defined

def main (x: i32) =
  let r = {a = x, b = x + 1, a = x + 2}
  in (r.a, r.b)
