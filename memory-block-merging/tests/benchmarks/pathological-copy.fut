-- Without memory coalescing, the generated code will copy from the memory of
-- xs' to double_buffer_mem.  With memory coalescing, it will not.
--
-- Adjust the number of elements to copy/not copy, and the number of iterations
-- to run.
--
-- (In any case, this is a very silly program, since it calculates the same
-- thing over and over in every iteration.  Watch out for optimisations that
-- notice this, both in Futhark and in gcc!)
-- ==
-- compiled input { 1000 1000 }
-- compiled input { 100000 1000 }
-- compiled input { 1000000 1000 }

let main (n_elements: i32, n_copies: i32): []i32 =
  let xs0 = iota n_elements
  in loop _xs = xs0 for _i < n_copies do
    let xs' = map (+ 1) xs0
    in xs'
