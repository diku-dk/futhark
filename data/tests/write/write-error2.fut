-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank.
-- ==
-- error:

fun [n]i32
  main([k]i32 indexes,
       [k]f32 values,
       *[n]i32 array) =
  write(indexes, values, array)
