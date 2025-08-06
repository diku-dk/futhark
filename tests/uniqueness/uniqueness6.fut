-- ==
-- input {
--   [1,2,3]
-- }
-- output {
--   [1,2,3]
-- }
def main [n] (arr: [n]i32) : []i32 =
  let newarr =
    (let notused = arr
     in replicate n 0)
  let newarr[0] = 0
  in arr
