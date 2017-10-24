-- ==
-- input {
-- }
-- output {
--   [1, 5, 9]
-- }
let main(): []i32 =
  let arr = [(0,1), (2,3), (4,5)]
  let n = length arr
  let outarr = replicate n (0,0) in
  let outarr = loop outarr = outarr for i < n do
    let outarr[i] = arr[i]
    in  outarr
  in map (\(x,y) -> x + y) outarr
