-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [12, 27]
-- }
fun [m]int main ([m][n]int xss, [m][n]int yss) =
    let final_res =
      map(fn int ([n]int xs, [n]int ys) =>
            let tmp =
              map (fn int (int x, int y) => x+y
                  , zip (xs,ys)) in
            reduce(+,0,tmp)
         , zip(xss,yss))
    in final_res
