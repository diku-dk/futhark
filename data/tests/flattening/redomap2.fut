-- ==
-- input {
--   [1,2,3]
--   [6,7,8]
-- }
-- output {
--   27
-- }
fun int main ([int,n] xs, [int,n] ys) =
  let tmp =
    map (fn int (int x, int y) => x+y
        , zip (xs,ys)) in
  reduce(+,0,tmp)
