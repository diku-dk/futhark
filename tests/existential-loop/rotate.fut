-- Permutations of the index functions require a copy
-- ==
-- input { [2, 1000, 42, 1001, 50000] }
-- output { [42, 1001, 50000, 2, 1000] }
-- structure gpu-mem { Copy 2 }

let main [n] (a: [n]i32): []i32 =
  loop xs = a for i < a[0] do
    rotate 1 xs
