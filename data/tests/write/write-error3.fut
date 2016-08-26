-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank, this time with multiple arrays in each field.
-- ==
-- error:

fun main(indexes: [k]i32,
       values0: [k]i32,
       values1: [k]f32,
       array1: *[n]i32,
       array2: *[n]i32): [n]i32 =
  write(indexes, zip(values0, values1), array1, array2)
