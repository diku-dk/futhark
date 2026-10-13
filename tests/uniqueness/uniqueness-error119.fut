-- A source must be a function of a single parameter.  The result of "uncurry2"
-- is not refined, and so aliases what "f" captures.
-- ==
-- error: consumed

def uncurry2 'a 'b 'c (f: a -> b -> c) (x: a) (y: b) : c =
  f x y

def main (x: i32) =
  let zs = replicate 3 x
  let f = \(y: i32) (w: i32) : *[3]i32 -> map (+ (y + w)) zs
  let r = uncurry2 f 1 2
  let r[0] = 0
  in (r, zs)
