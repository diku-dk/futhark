-- No cheating uniqueness just by using a clever name for the array.
-- ==
-- error: consumed

def main ((++): *[]i32) =
  let _ = (++) with [0] = 0
  in (++)
