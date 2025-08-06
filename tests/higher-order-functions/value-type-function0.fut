-- Since we distinguish between value types and lifted types, it
-- should not be possible to instantiate a polymorphic funtion, that
-- uses its (value type) polymorphic arguments in the branches of a
-- conditional, with a function type.
-- ==
-- error: functional

def cond 'a (b: bool) (x: a) (y: a) : a =
  if b then x else y

def main (b: bool) : i32 =
  let f = cond b (\(x: i32) -> x + x) (\(x: i32) -> x)
  in f 42
