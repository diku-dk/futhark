-- Vectorised reverse-mode AD (mjp) of a scatter that unrolls into a plain
-- (non-map) update_acc, because the array is statically size 1.  This used to
-- crash the compiler in the AD pass with a shape mismatch, because the free
-- variable adjoints returned from the return-sweep with_acc were given their
-- primal types instead of the vectorised adjoint types.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [3.0f32] }
-- output { [[1.0f32], [2.0f32]] }

def f (xs: [1]f32) : f32 =
  f32.sum (scatter (replicate 1 0.0) [0i64] xs)

entry main (xs: [1]f32) = mjp f xs [1.0, 2.0]
