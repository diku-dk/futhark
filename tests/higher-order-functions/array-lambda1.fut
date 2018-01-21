-- We cannot have an array containing literal lambda-expressions.
-- ==
-- error: Cannot form an array with elements of type .* -> .*
let main () : i32 = let _ = [\(x:i32) -> x+1, \(x:i32) -> x+x]
                    in 42
