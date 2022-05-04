-- | Definitions related to automatic differentiation.
--
-- **Warning:** This is an experimental feature.  It has several known
-- bugs.  Documentation is very sparse.  You probably don't want to
-- use this unless you have the soul of a hero.

-- | Jacobian-Vector Product ("forward mode"), producing also the
-- primal result as the first element of the result tuple.
let jvp2 'a 'b (f: a -> b) (x: a) (x': a) : (b, b) =
  intrinsics.jvp2 (f, x, x')

-- | Vector-Jacobian Product ("reverse mode"), producing also the
-- primal result as the first element of the result tuple.
let vjp2 'a 'b (f: a -> b) (x: a) (y': b) : (b, a) =
  intrinsics.vjp2 (f, x, y')

-- | Jacobian-Vector Product ("forward mode").
let jvp 'a 'b (f: a -> b) (x: a) (x': a) : b =
  (jvp2 f x x').1

-- | Vector-Jacobian Product ("reverse mode").
let vjp 'a 'b (f: a -> b) (x: a) (y': b) : a =
  (vjp2 f x y').1
