-- A size expression cannot consume anything.
-- ==
-- error: Consuming "ys", which is not consumable

def main (ys: *[]i32) =
  let zs = replicate 3 0i32 :> [let _ = ys with [0] = 1 in 3]i32
  in (zs, ys)
