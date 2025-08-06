-- ==
-- input {
--   [0, 1, -2, 5, 42]
--   [false, true, true, false, true]
-- }
-- output {
--   [true, true, true]
--   [1, -2, 42]
-- }
def main (xs1: []i32) (xs2: []bool) : ([]bool, []i32) =
  let tmp =
    filter (\(x: (i32, bool)) : bool ->
              let (i, b) = x in b)
           (zip xs1 xs2)
  in unzip (map (\(x: (i32, bool)) : (bool, i32) ->
                   let (i, b) = x in (b, i))
                tmp)
