-- When an loop contains a use of an array whose memory is allocated before the
-- loop, it must not reuse that memory, because there are cycles in loops.  This
-- program has such a case, where acc is copied inside the loop.
--
-- FIXME: This is not detected by the compiler yet.  The last use analysis needs
-- to be changed so that the last use of the memory of acc is reported for the
-- entire loop and *not* for the statement inside the loop where it is seemingly
-- lastly used.
-- ==
-- input {
--   [[[1,7],[9,4],[8,6]],
--    [[1,0],[6,4],[1,6]]]
-- }
-- output {
--   [[18, 17], [8, 10]]
-- }

-- structure cpu { Alloc 3 }

let main (a: [#n][#m][#k]i32): [n][k]i32 =
  let acc = replicate k 0 in
  map (\a_r ->
         loop acc for i < m do
           map (+) acc a_r[i]
      ) a
