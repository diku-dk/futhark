-- Nope, this one is also not OK (although it would be possible to
-- change the type system so that it would be).
-- ==
-- error: consumption

def zero (xs: *[]i32) (i: i32) =
  xs with [i] = 0

def apply f x = f x

def main (arr: *[]i32) =
  let f = zero arr
  in f 0
