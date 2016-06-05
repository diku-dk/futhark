-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank, this time with multiple arrays in each field.
-- ==
-- error:

fun [i32, n]
  main([i32, k] indexes,
       [i32, k] values0,
       [f32, k] values1,
       *[i32, n] array1,
       *[i32, n] array2) =
  write(indexes, zip(values0, values1), array1, array2)
