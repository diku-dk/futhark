-- An existential size in an apply function returning an unlifted type is not fine.
-- ==
-- error: existential

let apply 'a 'b (f: a -> b) (x: a): b =
  f x

let main (n: i32) = apply iota n
