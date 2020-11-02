-- Fusion must also happen to constants
-- ==
-- structure { Screma 1 }

let n = 1000 : i64
let x = map (+2) (map (+3) (iota n))

let main = x
