-- Record update of functional value.
-- ==
-- input { 1 2 } output { 3 2 }

def main (x: i32) (y: i32) : (i32, i32) =
  let r0 = {a = (+ x), b = (+ y)}
  let r1 = r0 with a = (\v -> r0.a v + y)
  in (r1.a 0, r1.b 0)
