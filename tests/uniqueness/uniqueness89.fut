-- Parametricity refines each component of a tuple result: both components come
-- from calls of functions that construct their results freshly, so they alias
-- nothing and can be consumed independently.  See Note [Parametric results] in
-- Language.Futhark.TypeChecker.Consumption.
-- ==
-- input { 2 }
-- output { [0,2,2,2,2,2,2,2,2,2] [0,2,2,2,2] }

def apply2 'a 'b 'c (f: a -> b) (g: a -> c) (x: a) : (b, c) =
  (f x, g x)

entry main (x: i32) =
  let (a, b) = apply2 (replicate 10) (replicate 5) x
  in ( a with [0] = 0
     , b with [0] = 0
     )
