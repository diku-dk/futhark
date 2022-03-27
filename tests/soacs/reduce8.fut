-- This test tests an explicitly commutative reduction over scalars.
-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   362880
-- }
def main(a: []i32): i32 = reduce_comm (*) 1 a
