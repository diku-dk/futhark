-- A shape declaration referring to a non-integer value should be an
-- error.
--
-- ==
-- error: declaration.*integer

let main(as: []i32, b: bool): [][]i32 =
  map (\i: [b]i32 -> replicate 3 i) as
