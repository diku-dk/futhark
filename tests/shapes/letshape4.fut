-- The monomorphiser forgot to keep around the 'n' in this program at
-- one point.

let n = 1
let vec 't arr = arr : [n]t
let main (xs: []i32) = vec xs
