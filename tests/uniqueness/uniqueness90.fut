-- A type parameter may have several sources.  The result of "choose" is the
-- result of calling either "f" or "g", and both construct their results
-- freshly, so consuming it does not consume what they capture.
-- ==
-- input { 1 }
-- output { [0,3,3] [1,1,1] }

def choose 'a 'b (c: bool) (f: a -> b) (g: a -> b) (x: a) : b =
  if c then f x else g x

entry main (x: i32) =
  let zs = replicate 3 x
  let f = \(y: i32) : *[3]i32 -> map (+ y) zs
  let g = \(y: i32) : *[3]i32 -> map (* y) zs
  let r = choose true f g 2
  let r[0] = 0
  in (r, zs)
