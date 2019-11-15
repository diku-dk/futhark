-- It is an error if the size of an array parameter depends on a later
-- parameter.  Written in a convoluted way to ensure this is checked
-- even for lambdas that are never let-generalised.
-- ==
-- error: refers to size `n`

let f : i32 = const 2 ((\xs n -> (zip xs (iota n) : [](i32, i32))))
