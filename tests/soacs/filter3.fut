-- ==
-- input {
--   [0, 1, -2, 5, 42]
--   [false, true, true, false, true]
-- }
-- output {
--   [true, true, true]
--   [1, -2, 42]
-- }
fun main(xs1: []int, xs2: []bool): ([]bool,[]int) =
  let tmp = filter (\(x: (int,bool)): bool  ->
                     let (i,b) = x in b
                  ) (zip xs1 xs2) in
  unzip(map (\(x: (int,bool)): (bool,int)  ->
              let (i,b) = x in (b,i)
           ) tmp)
