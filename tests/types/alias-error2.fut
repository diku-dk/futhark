-- No unique non-arrays
--
-- ==
-- error: .*non-array.*

type t = i32

let main(x: *t): t = x
