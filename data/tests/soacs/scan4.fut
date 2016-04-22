-- More complicated scanomap example.  Distilled from radix sort.
-- ==
--
-- input {
--   [83, 1, 4, 99, 33, 0, 6, 5]
-- }
-- output {
--   [0, 1, 4, 5, 6, 33, 83, 99]
-- }

fun [i32, n] main([i32, n] xs) =
  loop (xs) = for i < 2 do
    step(xs)
  in xs

fun [i32, n] step([i32, n] xs) =
  let bits = map(+1, xs)
  let ps1 = scan(+, 0, bits)
  let bits_sum = reduce(+, 0, bits)
  let ps1' = map(+bits_sum, ps1)
  let xs' = zipWith(+, ps1', xs)
  in xs'
