-- Validation of flattening with 2 lists
--
-- ==
-- entry: validate_flattening
-- input {[0i64, 1i64, 2i64, 3i64, 4i64, 5i64]}
-- output {[0i64, 0i64, 2i64, 6i64, 12i64, 20i64]}
entry validate_flattening (ns: []i64) : []i64 =
    map (\n -> i64.sum (opaque (iota n `concat` iota n))) ns