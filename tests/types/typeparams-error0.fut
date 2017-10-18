-- Missing parameter to a parametric type.
-- ==
-- error: vector

type vector 't = []t

let main(n: i32): vector = iota n
