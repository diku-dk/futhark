-- We cannot return a function from a pattern match.
-- ==
-- error: returned from pattern match

let f (x:i32) : i32 = x+x
let g (x:i32) : i32 = x+1

let main (b : bool) (n : i32) : i32 = (match b case _ -> f) n
