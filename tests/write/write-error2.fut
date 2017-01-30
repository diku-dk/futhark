-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank.
-- ==
-- error:

fun main(indexes: [k]i32,
       values: [k]f32,
       array: *[n]i32): [n]i32 =
  write indexes values array
