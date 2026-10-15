-- Recursion is monomorphic, so the result of a recursive call of a
-- polymorphic function is protected by parametricity like any other
-- application, and can be consumed.
-- ==
-- input { 2i64 [1,2,3] } output { [3,3,3] }

def f 'a (n: i64) (xs: *[]a) : []a =
  if n == 0
  then xs
  else let ys = f (n - 1) xs
       let v = copy ys[2]
       in ys with [n - 1] = v

entry main (n: i64) (xs: *[]i32) = f n xs
