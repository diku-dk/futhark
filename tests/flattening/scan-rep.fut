-- Validation of flattening with 3 lists
--
-- ==
-- entry: validate_flattening2
-- input {[0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 10i64, 13i64]}
-- output {[0i64, 1i64, 4i64, 9i64, 16i64, 25i64, 100i64, 169i64]}
entry validate_flattening2 (ns: []i64) : []i64 =
    map (\n -> i64.sum (opaque ((replicate n 1) `concat` iota n `concat` iota n))) ns