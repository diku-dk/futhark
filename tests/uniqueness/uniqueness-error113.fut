-- An in-place update through record fields keeps the other components, so
-- they cannot share memory with the updated one.
-- ==
-- error: shares memory with

def main (u: *[3]i32) =
  let r = {x = {y = u}, z = u}
  let r.x.y[0] = 1
  in r.z
