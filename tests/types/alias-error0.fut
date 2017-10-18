-- No circular types!
--
-- ==
-- error: Unknown type

type t = t

let main(x: t): t = x
