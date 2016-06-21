-- More complicated scanomap example.  Distilled from radix sort.
-- ==
--
-- input {
--   [83, 1, 4, 99, 33, 0, 6, 5]
-- }
-- output {
--   [4339, 4586, 4929, 5654, 6120, 6554, 7046, 7535]
-- }

fun [n]i32 main([n]i32 xs) =
  loop (xs) = for i < 2 do
    step(xs)
  in xs

fun [n]i32 step([n]i32 xs) =
  let bits = map(+1, xs)
  let ps1 = scan(+, 0, bits)
  let bits_sum = reduce(+, 0, bits)
  let ps1' = map(+bits_sum, ps1)
  let xs' = zipWith(+, ps1', xs)
  in xs'
