-- This loop nest was derived from an LU factorisation program, and
-- exposed a bug in a simplification rule.  It does not compute
-- anything interesting.
--
-- Specifically, the bug was in the detection of loop-invariant
-- variables - an array might be considered loop-invariant, even
-- though some of its existential parameters (specifically shape
-- arguments) are not considered loop-invariant (due to missing copy
-- propagation).
-- ==

def main [n] (a: *[n][]f64, u: *[][]f64) : ([][]f64, [][]f64) =
  loop ((a, u)) for k < n do
    let u[k, k] = a[k, k]
    let a = loop (a) for i < n - k do a
    in (a, u)
