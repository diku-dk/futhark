-- This test checks whether we can consume something in a loop,
-- without causing an error just because it's aliased outside the loop.
-- ==
-- input {
-- }
-- output {
--   0
-- }

def main : i32 =
  let n = 10
  let inarr = replicate n 0
  let _ =
    loop outarr = inarr
    for i < n do
      if i == 0
      then outarr
      else let outarr[i] = i in outarr
  in 0
