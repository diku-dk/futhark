-- A shape declaration referring to a non-integer value should be an
-- error.
--
-- ==
-- error: bool

def main (as: []i32, b: bool) : [][]i32 =
  map (\i : [b]i32 -> replicate 3 i) as
