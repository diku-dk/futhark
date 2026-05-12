def main (x: i32) (y: i32) =
  let t = (x, y)
  let f g = g t.0
  in f (+ 2)
