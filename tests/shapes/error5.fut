-- A function 'a -> a' must be size-preserving.
-- ==
-- error: Occurs check

def ap 'a (f: a -> a) (x: a) =
  f x

def main [n] (arr: [n]i32) =
  ap (\xs -> xs ++ xs) arr
