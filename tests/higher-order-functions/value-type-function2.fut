-- A lifted type parameter cannot be used as the type of the branches
-- of a conditional.
-- ==
-- error:

def cond '^a (b: bool) (x: a) (y: a) : a =
  if b then x else y

def main (b: bool) (y: i32) : i32 =
  let f = cond b (\(x: i32) -> x + x) (\(x: i32) -> x)
  in f y
