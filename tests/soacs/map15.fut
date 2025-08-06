-- Test that a map not using its parameters can be turned into a
-- replicate.
--
-- ==
-- input { 2 [1,2,3] }
-- output { [4, 4, 4] }
-- structure { Map 0 Replicate 1 }

def main (x: i32) (a: []i32) : []i32 =
  map (\(y: i32) : i32 ->
         x + 2)
      a
