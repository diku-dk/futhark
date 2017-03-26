-- This test checks whether we can consume something in a loop,
-- without causing an error just because it's aliased outside the loop.
-- ==
-- input {
-- }
-- output {
--   0
-- }

let main(): i32 =
  let n = 10
  let inarr  = copy(replicate n 0) in
  loop ( outarr  = inarr ) = for i < n  do
      if i == 0
      then outarr
      else let outarr[i] = i in outarr
  in 0
