// This test checks whether we can consume something in a loop,
// without causing an error just because it's aliased outside the loop.

fun int main() =
  let n = 10 in
  let inarr  = copy(replicate(n, 0)) in
  loop ( outarr  = inarr ) = for i < n  do
      if i == 0
      then outarr
      else let outarr[i] = i in outarr
  in 0
