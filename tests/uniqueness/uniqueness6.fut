-- ==
-- input {
--   [1,2,3]
-- }
-- output {
--   [1,2,3]
-- }
let main(arr:  [#n]i32): []i32 =
  let  newarr  =
       (let notused  = arr
        in copy(replicate n 0))
  let newarr[0] = 0 in
  arr
