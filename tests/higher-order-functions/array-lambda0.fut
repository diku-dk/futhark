-- We cannot have an array containing a literal lambda-expression.
-- ==
-- error: functional
let main : i32 = let _ = [\(x:i32) -> x+1]
                 in 42
