-- A loop consumes its initial value when it starts, but every iteration uses
-- the array it iterates over, so the two may not alias.
-- ==
-- error: Argument is consumed, but aliases

def main (xss: *[][]i32) =
  loop yss = xss for xs in xss do yss with [1] = map2 (+) yss[1] xs
