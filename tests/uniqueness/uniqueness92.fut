-- The freshness of a lambda is inferred, and parametricity sees it: the results
-- of "apply2" and "|>" are fresh although no lambda declares a return type.
-- See Note [Parametric results] in Language.Futhark.TypeChecker.Consumption.
-- ==
-- input { 1 }
-- output { [0,3,3] [0,2,2] [0,-1,-1] [1,1,1] }

def apply2 'a 'b 'c (f: a -> b) (g: a -> c) (x: a) : (b, c) =
  (f x, g x)

entry main (x: i32) =
  let zs = replicate 3 x
  let (a, b) = apply2 (\y -> map (+ y) zs) (\y -> map (* y) zs) 2
  let c = 2 |> (\y -> map (\z -> z - y) zs)
  let a[0] = 0
  let b[0] = 0
  let c[0] = 0
  in (a, b, c, zs)
