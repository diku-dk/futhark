-- Type-changing record update.
-- ==
-- error: i32.*bool

def main (x: i32) (y: i32) : (bool, i32) =
  let r0 = {x, y}
  let r1 = r0 with x = true
  in (r1.x, r1.y)
