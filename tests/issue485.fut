-- Avoid fusing the map into the scatter, because the map is reading
-- from the same array that the scatter is consuming.  The
-- complication here is that the scatter is actually writing to an
-- *alias* of the array the map is reading from.


let main (n: i32) (m: i32) =
  let xss = map (\i -> map (\j -> i * j) (iota m)) (iota n)
  let xs = reshape (n*m) xss -- now xs aliases xss
  let vs = map (\i -> xss[i%n,i%m]) (iota (n*m)) -- read from xss
  in scatter xs (iota (n*m)) vs -- consume xs
