-- We cannot have an array containing literal lambda-expressions.
-- ==
-- error: functional

let main () : i32 = let _ = [\(x:i32) -> x+1, \(x:i32) -> x+x]
                    in 42
