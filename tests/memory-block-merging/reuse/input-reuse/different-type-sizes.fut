-- Not enough space.  Checks that the compiler also compares primitive types.
-- ==
-- input { [2, 5, 9] }
-- output { [2i64, 5i64, 9i64] }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main (xs: *[]i32): []i64 =
  map i64.i32 xs
