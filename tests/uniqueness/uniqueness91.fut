-- A source may be a component of a tuple parameter.  The instance of "app" must
-- also declare the function in its parameter as returning a fresh value, or its
-- body would not justify its fresh result.
-- ==
-- input { 1 }
-- output { [0,3,3] [1,1,1] }

def app 'a 'b (p: (a -> b, a)) : b =
  p.0 p.1

entry main (x: i32) =
  let zs = replicate 3 x
  let f = \(y: i32) : *[3]i32 -> map (+ y) zs
  let r = app (f, 2)
  let r[0] = 0
  in (r, zs)
