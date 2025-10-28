-- let should not be generalised
-- ==
-- error: Cannot apply "apply"

def main x =
  let apply f x = f x
  in apply (apply (i32.+) x) x
