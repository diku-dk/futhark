-- A consuming function must not be passed as a higher-order argument!
-- ==
-- error: contains-consumption

def zero (xs: *[]i32) (i: i32) =
  xs with [i] = 0

def apply f x = f x

def main (arr: *[]i32) =
  let f = zero arr
  in apply f 0
