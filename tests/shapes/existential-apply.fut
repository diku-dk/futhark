-- An existential size in an apply function returning a lifted type is fine.
-- ==
-- input { 2 } output { [0,1] }

let apply 'a '^b (f: a -> b) (x: a): b =
  f x

let main (n: i32) = apply iota n
