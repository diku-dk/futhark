-- Vectorised reverse-mode AD (mjp) of a scatter that unrolls into a plain
-- (non-map) update_acc, because the array is statically size 1.  This used to
-- crash the compiler in the AD pass with a shape mismatch, because the free
-- variable adjoints returned from the return-sweep with_acc were given their
-- primal types instead of the vectorised adjoint types.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [3.0] }
-- output { [[1.0], [2.0]] }

def f (xs: [1]f64) : f64 =
  f64.sum (scatter (replicate 1 0.0) [0i64] xs)

entry main (xs: [1]f64) = mjp f xs [1.0, 2.0]
