-- This failed at one point during type-checking because the k was not
-- visible in the zipWith return type.

fun main(a: [n][m][k]int): [n][k]int =
  let acc_expanded = replicate(n, replicate(k, 0)) in
  loop(acc_expanded) = for i < m do
    zipWith(fn (acc: [k]int, a_r: [m][k]int): [k]int  =>
              zipWith(+, acc, a_r[i])
           , acc_expanded, a)
  in acc_expanded
