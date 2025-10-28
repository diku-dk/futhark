-- Polymorphic higher-order function with an argument lambda that
-- closes over a local variable.
-- ==
-- input { 5 true } output { 7 }
-- input { 5 false } output { 9 }

def twice 'a (f: a -> a) (x: a) : a = f (f x)

def main (x: i32) (b: bool) : i32 =
  twice (\(y: i32) -> if b then y + 1 else y + 2) x
