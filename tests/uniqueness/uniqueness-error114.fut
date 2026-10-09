-- As uniqueness-error113, but the components coincide in only one branch, and
-- through the update expression.
-- ==
-- error: shares memory with

def main (c: bool) (u: *[3]i32) (a: *[3]i32) (b: *[3]i32) =
  let r = if c then {x = {y = u}, z = u} else {x = {y = a}, z = b}
  in (r with x.y[0] = 99).z
