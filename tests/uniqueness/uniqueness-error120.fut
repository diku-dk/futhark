-- A source must observe its argument.  The result of "g" is not refined, and so
-- aliases what "f" captures.
-- ==
-- error: consumed

def g 'b (f: *[3]i32 -> b) (xs: *[3]i32) : b =
  f xs

def main (x: i32) =
  let zs = replicate 3 x
  let f = \(v: *[3]i32) : *[3]i32 -> v with [0] = zs[0]
  let r = g f (replicate 3 0)
  let r[0] = 0
  in (r, zs)
