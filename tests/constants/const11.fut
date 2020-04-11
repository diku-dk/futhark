-- Errors in parallelism in constants.
-- ==
-- input { 2 }
-- error: out of bounds

let n = 10
let arr = iota n
let bad = map (\i -> arr[if i == 0 then -1 else i]) (iota n)

let main x = map (+x) bad
