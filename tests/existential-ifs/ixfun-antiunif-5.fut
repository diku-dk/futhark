-- Another simple test for index-function anti-unification across an if-then-else
-- This one returns the same memory block, only the offset is existentialized.
-- ==
-- random input  { [30000]i32 }
-- auto output
-- structure gpu-mem { Manifest 0 }
def main (a: []i32) =
  let xs =
    if a[0] > 0
    then a[10:30:2]
    else a[5:20:3]
  in reduce (+) 0 xs
