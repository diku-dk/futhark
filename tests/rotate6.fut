-- This really tests a simplification rule for composing rotates and
-- rearranges.
--
-- ==
-- input { 1
--   [[[4i32, 4i32, 4i32], [9i32, 1i32, 8i32]], [[2i32, 2i32, 2i32],
--   [7i32, 4i32, 1i32]], [[3i32, 1i32, 5i32], [6i32, 1i32, 2i32]],
--   [[6i32, 3i32, 3i32], [7i32, 1i32, 6i32]]]
-- }
-- output { [[[3i32, 6i32, 4i32, 2i32], [6i32, 7i32, 9i32, 7i32]],
--   [[1i32, 3i32, 4i32, 2i32], [1i32, 1i32, 1i32, 4i32]], [[5i32,
--   3i32, 4i32, 2i32], [2i32, 6i32, 8i32, 1i32]]]
-- }


let main(i: i32, arr: [][][]i32): [][][]i32 =
  rotate@2 i (rearrange (2,1,0) (rotate i arr))
