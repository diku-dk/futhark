-- Fail if the value and I/O arrays do not have the same primitive type and
-- rank.
-- ==
-- error:

let main(indexes: [k]i32,
       values: [k]f32,
       array: *[n]i32): [n]i32 =
  write indexes values array
