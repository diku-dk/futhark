-- ==
-- input {
--   [[1,2,3],[1,2,3]]
--   [[3,2,1],[6,7,8]]
-- }
-- output {
--   [12, 27]
-- }
def main [m] [n] (xss: [m][n]i32) (yss: [m][n]i32) : [m]i32 =
  let final_res =
    map (\(xs: [n]i32, ys: [n]i32) : i32 ->
           let tmp =
             map (\(x: i32, y: i32) : i32 -> x + y)
                 (zip xs ys)
           in reduce (+) 0 tmp)
        (zip xss yss)
  in final_res
