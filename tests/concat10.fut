-- Simplifying away a redundant concat.
-- ==
-- structure { Concat 0 }

let main (xs: []i32) : *[]i32 =
  xs ++ []
