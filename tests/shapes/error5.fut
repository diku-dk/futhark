-- A function 'a -> a' must be size-preserving.
-- ==
-- error: do not match

let ap 'a (f: a -> a) (x: a) =
  f x

let main [n] (arr: [n]i32) =
  ap (\xs -> xs ++ xs) arr
