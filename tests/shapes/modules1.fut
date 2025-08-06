-- It is not allowed to create an opaque type whose size parameters
-- are not used in array dimensions.
-- ==
-- error: is not used constructively

module m = {
             type^ t [n] = [n]i32 -> i64
             def f [n] (_: t [n]) = 0
             def mk (n: i64) : t [n] = \(xs: [n]i32) -> n
           }:
           {
             type^ t [n]
             val f [n] : (x: t [n]) -> i32
             val mk : (n: i64) -> t [n]
           }

def main x = (x + 2) |> m.mk |> m.f
