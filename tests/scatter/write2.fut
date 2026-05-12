-- Test that write works in even more non-trivial cases.
-- ==
--
-- input {
--   [2, -1, 0]
--   [[[0, 0, 1], [5, 6, 7]],
--    [[0, 0, 0], [0, 0, 0]],
--    [[1, 1, 14], [15, 16, 17]]]
--   [[[1, 2, 3], [10, 20, 30]],
--    [[1, 2, 3], [10, 20, 30]],
--    [[14, 24, 34], [11, 21, 31]]]
-- }
-- output {
--   [[[1, 1, 14], [15, 16, 17]],
--    [[1, 2, 3], [10, 20, 30]],
--    [[0, 0, 1], [5, 6, 7]]]
-- }

def main [k] [t] [m] [n]
         (indexes: [k]i32)
         (values: [k][t][m]i32)
         (array: *[n][t][m]i32) : [n][t][m]i32 =
  scatter array (map i64.i32 indexes) values
