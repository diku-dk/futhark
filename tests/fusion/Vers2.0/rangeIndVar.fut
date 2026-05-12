-- ==
-- input {
--   [1,2,2,3,33,4,5,6,7,8,9,0]
-- }
-- output {
--   33
-- }

def main [m] (arr: [m]i32) : i32 =
  let k =
    loop k = 0
    for i < m - 1 do
      if i % 3 == 0
      then k + 1
      else k
  in arr[k]
