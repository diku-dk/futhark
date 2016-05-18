-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank.
-- ==
-- error:

fun [i32, n]
  main([i32, k] indexes,
       [f32, k] values,
       *[i32, n] array) =
  write(indexes, values, array)
