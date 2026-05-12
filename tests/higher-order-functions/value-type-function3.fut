-- We can not even use a lifted type parameter as the result type of a
-- condition even if it's not actually instantiated with a function type.
-- ==
-- error:

def cond '^a (b: bool) (x: a) (y: a) : a =
  if b then x else y

def main (b: bool) (x: i32) : i32 =
  cond b x 0
