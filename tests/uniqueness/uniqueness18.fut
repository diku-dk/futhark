-- When the map is simplified away, it must turn into a copy, as the
-- result is consumed.
--
-- ==
-- structure { Map 0 Replicate 1 }

def main (as: []i32) : *[]i32 =
  map (\x -> x) as
