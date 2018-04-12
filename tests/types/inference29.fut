-- Unification of inferred type with ascription in pattern.
-- ==
-- input { 2 } output { 2 }

let main x = let y: i32 = x in y
