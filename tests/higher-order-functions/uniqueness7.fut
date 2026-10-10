-- A partial application that consumes an argument holds it, even when the
-- argument is fresh, so it cannot be bound to a name.
-- ==
-- error: contains consumption

def zero (xs: *[]i32) (i: i32) =
  xs with [i] = 0

def uniq (x: i32) : *[]i32 = [x, x, x]

def main (x: i32) =
  let f = zero (uniq x)
  in f 0
