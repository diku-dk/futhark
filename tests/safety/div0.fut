-- Division by zero, and in a parallel context at that!
-- We ignore this test in for the ISPC backend, since it isn't possibly to
-- early-return from a kernel, so an eventual division by 0 will happen.
-- ==
-- no_ispc-multicore input { [0] } error:

def main (xs: []i32) = map (2/) xs
