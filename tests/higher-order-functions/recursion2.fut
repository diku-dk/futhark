-- A recursive function where lambda lifting results in mutually recursive
-- higher-order functions - the question is/was whether defunctionalisation can
-- cope with this.
-- ==
-- input { 10i64 }
-- output { 65i64 }

def f (g: i64 -> i64) (n: i64) : i64 =
  let h (x: i64) = g x + f g (n - 1)
  in if n == 0 then 0 else h n

entry main (n: i64) = f (\x -> x + 1) n
