-- A least significant digit radix sort to test out `write`; this variant
-- directly based on [1], which is apparently also the basis for one of
-- Accelerate's example programs.
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
-- ==
--
-- input {
--   [83, 1, 4, 99, 33, 0, 6, 5]
-- }
-- output {
--   [0, 1, 4, 5, 6, 33, 83, 99]
-- }
--
-- input @ radix_sort_100.in
-- output @ radix_sort_100.out

fun [u32, n] main([u32, n] xs) =
  split_radix_sort(xs, 32)

fun [u32, n] split_radix_sort([u32, n] A, i32 number_of_bits) =
  loop (A) = for i < number_of_bits do
    let Ai = map(fn i32 (u32 a) => i32((a >> u32(i)) & 1u32), A)
    in split_blelloch(A, Ai)
  in A

fun [u32, n] split_blelloch([u32, n] A, [i32, n] Flags) =
  let I_down = plus_prescan(map(1 -, Flags))
  let I_up = map(n -, plus_scan_reverse_order(Flags))
  let Index = map(fn i32 (i32 i) =>
                    if Flags[i] == 1 then I_up[i] else I_down[i],
                  iota(n))
  in permute(A, Index)

fun [i32, n] plus_scan_reverse_order([i32, n] X) =
  let Xreversed = map(fn i32 (i32 i) => X[n - i - 1], iota(n))
  let X' = plus_scan(Xreversed)
  let X'reversed = map(fn i32 (i32 i) => X'[n - i - 1], iota(n))
  in X'reversed

fun [i32, n] plus_scan([i32, n] X) =
  scan(+, 0, X)

fun [i32, n] plus_prescan([i32, n] X) =
  let Xshifted = map(fn i32 (i32 i) => if i == 0 then 0 else unsafe X[i - 1], iota(n))
  in scan(+, 0, Xshifted)

fun [u32, n] permute([u32, n] A, [i32, n] Index) =
  write(Index, A, copy(A))
