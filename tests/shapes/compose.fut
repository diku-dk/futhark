-- An argument for why we should not permit composition of functions
-- with anonymous return sizes.
-- ==
-- error: "compose".*"iota"

let compose '^a '^b '^c (f: a -> b) (g: b -> c) (x: a) (y: a): (c, c) =
  (g (f x), g (f y))

let main = compose iota (\x -> length x) 1 2
