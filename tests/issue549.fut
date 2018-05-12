-- Slightly too restrictive type checking in the core language.
-- ==

let main (n: i32) =
  let on_row (x: i32) = replicate x x
  let a = iota n
  in unsafe map on_row a
