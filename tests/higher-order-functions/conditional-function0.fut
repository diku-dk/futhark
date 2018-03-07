-- We cannot return a function from a conditional.
-- ==
-- error: Branches of conditional .* only allowed to have base type.

let f (x:i32) : i32 = x+x
let g (x:i32) : i32 = x+1

let main (b : bool) (n : i32) : i32 = (if b then f else g) n
