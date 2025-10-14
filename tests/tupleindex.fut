-- ==
-- input {
-- }
-- output {
--   0
--   1
-- }
def main : (i32, i32) =
  let arr = [(0, 1), (2, 3), (4, 5)]
  let n = length arr
  let outarr = replicate n (0, 0)
  let i = 0
  let outarr[i] = arr[i]
  in outarr[0]
