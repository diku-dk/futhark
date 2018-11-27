-- This should not warn, even though it is partially applied.
-- ==
-- warning: ^$
let main = reduce (i32.+) 0
