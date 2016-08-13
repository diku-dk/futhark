-- ==
-- input {
--   [0, 1, -2, 5, 42]
--   [False, True, True, False, True]
-- }
-- output {
--   [True, True, True]
--   [1, -2, 42]
-- }
fun ([]bool,[]int) main([]int xs1, []bool xs2) =
  let tmp = filter(fn bool ((int,bool) x) =>
                     let (i,b) = x in b
                  , zip(xs1,xs2)) in
  unzip(map(fn (bool,int) ((int,bool) x) =>
              let (i,b) = x in (b,i)
           , tmp))
