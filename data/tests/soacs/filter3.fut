-- ==
-- input {
--   [0, 1, -2, 5, 42]
--   [False, True, True, False, True]
-- }
-- output {
--   [True, True, True]
--   [1, -2, 42]
-- }
fun main(xs1: []int, xs2: []bool): ([]bool,[]int) =
  let tmp = filter (fn (x: (int,bool)): bool  =>
                     let (i,b) = x in b
                  ) (zip(xs1,xs2)) in
  unzip(map (fn (x: (int,bool)): (bool,int)  =>
              let (i,b) = x in (b,i)
           ) tmp)
