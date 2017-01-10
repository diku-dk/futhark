-- Segmented sum, non commutative
-- ==
-- input {
--   [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32]]
-- }
-- output {
--   [6.0f32, 15.0f32]
-- }

-- Having this a seperate function currently makes the compiler not realize that
-- adition is actually a commutative operator. This trick might not last forever.
fun add (x : f32) (y : f32): f32 = x + y

fun main (xss : [m][n]f32): [m]f32 =
  map (fn xs => reduce add 0.0f32 xs) xss
