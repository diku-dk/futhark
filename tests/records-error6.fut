-- It is not OK to match the same field twice.
-- ==
-- error: Duplicate fields

def main (x: i32) =
  let {x = a, x = b} = {x}
  in a + b
