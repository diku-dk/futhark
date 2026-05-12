-- Verify that we can perform an in-place update on the result of a
-- transposition.
-- ==

def main (xss: *[][]i32) =
  let xss' = transpose xss
  in xss' with [0, 1] = 2
