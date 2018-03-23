-- Inferring a function parameter into a tuple in an interesting way.
-- ==
-- input { 1 2 } output { 1 2 }

let curry f x y = f (x, y)
let id x = x

let main (x: i32) (y: i32) = curry id x y
