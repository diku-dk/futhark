-- A value type parameter of a polymorphic function cannot be instantiated to a
-- function type by passing the function as an argument to another function.
-- ==
-- error: functional

def app (f: (i32 -> i32) -> (i32 -> i32)) : i32 =
  f (\(x: i32) -> x + x) 42

def id 'a (x: a) : a = x

def main : i32 = app id
