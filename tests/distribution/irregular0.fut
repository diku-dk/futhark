-- Very irregular parallelism.  Should not distribute, but just
-- sequentialise body.
--
-- ==
-- input { [1,2,3,4,5,6,7,8,9] }
-- output { [1, 3, 6, 10, 15, 21, 28, 36, 45] }
-- structure distributed {
--   Kernel/GroupReduce 2
--   Kernel/GroupStream 1
-- }

let main(a: []i32): []i32 =
  map (\(i: i32): i32  ->
        reduce (+) 0 (map (+1) (iota(i)))) a
