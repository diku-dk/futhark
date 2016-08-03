-- ==
-- input {
-- }
-- output {
--   0
--   1
-- }
fun (int, int) main() =
  let arr = [(0,1), (2,3), (4,5)] in
  let n = shape(arr)[0] in
  let outarr = replicate(n, (0,0)) in
  let i = 0 in
  let outarr[i] = arr[i]
  in  outarr[0]
