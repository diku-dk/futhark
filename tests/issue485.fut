-- Avoid fusing the map into the scatter, because the map is reading
-- from the same array that the scatter is consuming.  The
-- complication here is that the scatter is actually writing to an
-- *alias* of the array the map is reading from.


def main (n: i64) (m: i32) =
  let xs = iota n
  let ys = xs : *[n]i64 -- now ys aliases xs
  let vs = map (\i -> xs[(i+2)%n]) (iota n) -- read from xss
  in scatter ys (iota n) vs -- consume xs
