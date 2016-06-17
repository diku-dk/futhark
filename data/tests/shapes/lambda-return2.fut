-- This failed at one point during type-checking because the k was not
-- visible in the zipWith return type.

fun [n][k]int main([n][m][k]int a) =
  let acc_expanded = replicate(n, replicate(k, 0)) in
  loop(acc_expanded) = for i < m do
    zipWith(fn [k]int ([k]int acc, [m][k]int a_r) =>
              zipWith(+, acc, a_r[i])
           , acc_expanded, a)
  in acc_expanded
