-- ==
-- input {
-- }
-- output {
--   [1, 5, 9]
-- }
fun [int] main() =
  let arr = [(0,1), (2,3), (4,5)] in
  let n = size(0, arr) in
  let outarr = replicate(n, (0,0)) in
  loop (outarr = outarr) = for i < n do
    let outarr[i] = arr[i]
    in  outarr
  in map(+, outarr)
