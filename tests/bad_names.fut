-- Tests that the code generator does not choke on terrible names that
-- are not valid in C.
--
-- ==
-- input { false 2 }
-- output { 12 }

fun main(r': bool, x_: i32): i32 =
  if r' then 123 else (x_ + 1) * 2 + 6
