-- Just because a type parameter *may* be function, it does not *have*
-- to be a function.
-- ==
-- input { 2 } output { [2] }

let id '^a (x: a) = x

let array 't (f: t -> i32) (t: t) = [f t]

let main (x: i32) = array id x
