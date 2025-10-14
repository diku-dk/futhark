-- Segmented sum, non commutative
-- ==
-- input {
--   true
--   [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32]]
-- }
-- output {
--   [6.0f32, 15.0f32]
-- }

-- Add a data-driven branch to prevent the compiler from noticing that
-- this is commutative.
def add (b: bool) (x: f32) (y: f32) : f32 = if b then x + y else x - y

def main [m] [n] (b: bool) (xss: [m][n]f32) : [m]f32 =
  map (\xs -> reduce (add b) 0.0f32 xs) xss
