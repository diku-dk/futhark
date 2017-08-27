-- Partial array creations inside kernels.  No memory should be reused for this
-- program.  The 'structure' annotation is not too useful, as this program gets
-- transformed a lot and gets many new memory blocks, but it should help with
-- discovering irregularities.  Also note that we only test the gpu structure;
-- this is because the case we are interested in only occurs with kernels.
--
-- The large number of allocations is due to many small kernel-supporting memory
-- blocks.  It might change if the compiler changes.  This is also the case with
-- the other tests in this directory.
-- ==
-- structure gpu { Alloc 20 }

-- If compiled with the GPU pipeline, this program will end up with these two
-- array creations inside the same kernel:
--
--   result_first: [chunk][m]f32@mem_first->Direct(num_groups, chunk, m, group_size)[0, 3, 1, 2][group_id, local_tid, 0i64:+chunk*1i64, 0i64:+m*1i64]
--
--   result_second: [o][m]f32@mem_second->Direct(num_groups, o, m, group_size)[0, 3, 1, 2][group_id, local_tid, 0i64:+o*1i64, 0i64:+m*1i64]
--
-- Both create an array, but neither create the *entire* array: They each have
-- an index in their index functions, and they don't match: result_first indexes
-- into 'chunk', and result_second indexes into 'o'.
--
-- The last use of result_first is before result_second, so naively they do not
-- interfere.  However, since the creations occur inside kernels, and since they
-- write to different parts of their thread-local arrays, they do actually
-- interfere.  If result_second were writing to the same index range as
-- result_first, it would be okay to merge their two memory blocks, but in this
-- case we might end up with a program execution like this if we merge them:
--
--   Value a is written to result_first in thread t.
--   Value b is written to result_second in thread u.
--   Value b is read from result_first in thread t.
--
-- So, a race condition.  This happens because writing to result_second in
-- thread u can be the same as writing to result_first in thread t.  We solve
-- this by extending the interference analysis to always record interference
-- between two array memory blocks if the array creations use indices, and the
-- indices are not the same.  This requirement can probably be made less
-- conservative.
--
-- This test is a simplified variant of tests/streamRed_interchange.fut (which
-- is in turn inspired by the kmeans benchmark).

let main (m: i32, n: i32, o: i32): [o][m]f32 =
  let membership = map (% o) (iota n)
  let values = map (\i -> map f32 (map (^ i) (iota m))) (iota n) in

  let f
    (acc: *[#o][#m]f32)
    (elem: *[#o][#m]f32): *[o][m]f32 =
    map (\xs ys -> map (+) xs ys) acc elem

  let g
    (inp: [#chunk]([#m]f32, i32)): *[o][m]f32 =
    loop acc = replicate o (replicate m 0.0f32) for i < chunk do
      let (values, c) = inp[i] in
      unsafe let acc[c] = map (+) acc[c] values
    in acc

  in stream_red_per f g (zip values membership)
