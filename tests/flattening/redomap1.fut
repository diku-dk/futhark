-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [12, 27]
-- }
fun main (xss: [m][n]int, yss: [m][n]int): [m]int =
    let final_res =
      map (\(xs: [n]int, ys: [n]int): int  ->
            let tmp =
              map  (\(x: int, y: int): int  -> x+y
                  ) (zip  xs ys) in
            reduce (+) 0 tmp
         ) (zip xss yss)
    in final_res
