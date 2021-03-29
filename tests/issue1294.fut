-- ==
-- error: array element

let apply 'a '~b (f: i64 -> a -> b) (x: a) =
  [f 0 x, f 1 x]

let main (x: i32) = apply replicate x
