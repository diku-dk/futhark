-- A consumed argument may not alias the closure of the function it is passed to.
-- ==
-- error: aliases the function

def main (x: *[]i32) : i32 =
  let f = \(y: *[]i32) : i32 -> let y[0] = 10 in y[0] + x[0]
  in f x
