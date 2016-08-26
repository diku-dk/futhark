-- Test that top-to-bottom loops work.
-- ==
-- input { 3 [1,2,3,4,5] }
-- output { [5, 4] }
-- input { 5 [1,2,3,4,5] }
-- output { empty(int) }

fun main(l: int, a: [n]int): []int =
  loop (b = replicate(n-l, 0)) = for n > i >= l do
    let j = n - i - 1 in
    let b[j] = a[i] in
    b
  in b
