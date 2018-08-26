-- Simplification of concatenations of replicates and array literals.
-- ==
-- structure { Replicate 0 ArrayLit 1 Concat 0 }

let main (a: i32) (b: i32) (c: i32) =
  [a] ++ [b] ++ [c]
