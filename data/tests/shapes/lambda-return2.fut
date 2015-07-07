-- This failed at one point during type-checking because the k was not
-- visible in the zipWith return type.

fun [[int,k],n] main([[[int,k],m],n] a) =
  let acc_expanded = replicate(n, replicate(k, 0)) in
  loop(acc_expanded) = for i < m do
    zipWith(fn [int,k] ([int,k] acc, [[int,k],m] a_r) =>
              zipWith(+, acc, a_r[i])
           , acc_expanded, a)
  in acc_expanded
