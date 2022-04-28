-- If statements that return arrays can also be migrated if doing so does not
-- introduce any additional array copying.
--
-- In this case the outer 'if' can be migrated because all possible return
-- values originate from array literals declared within its branches. These
-- array literals would all be migrated to a GPUBody anyway, so migrating the
-- if statement introduces no additional copying.
-- ==

-- structure gpu {
--   ArrayLit 3
--   /GPUBody/If 1
--   /ArrayLit 0
--   /If 0
-- }

-- This fails due to a memory allocation error.

-- def main (A: [2]i64) (x: i64) : [2]i64 =
--   if A[0] == 0
--   then [opaque x, 1]
--   else let B = opaque [opaque x, 2, 0]
--        in if A[1] == 1
--           then loop _ = B[0:2] for i < x do [i, 3]
--           else [opaque x, 4]
