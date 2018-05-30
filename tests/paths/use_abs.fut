-- Test that indirectly including a file with '..' does not result in
-- that file being imported twice (verified by using an abstract
-- type).
-- ==
-- input {} output { true }

module has_abs = import "has_abs"
module subdir_has_abs = import "subdir/has_abs"

let main = has_abs.eq has_abs.x subdir_has_abs.x
