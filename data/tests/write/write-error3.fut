-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank, this time with multiple arrays in each field.
-- ==
-- error:

fun [n]i32
  main([k]i32 indexes,
       [k]i32 values0,
       [k]f32 values1,
       *[n]i32 array1,
       *[n]i32 array2) =
  write(indexes, zip(values0, values1), array1, array2)
