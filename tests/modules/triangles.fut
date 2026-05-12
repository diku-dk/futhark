-- ==
-- input {
--   10 21 21
--   19 12 5
-- }
-- output {
--   547
-- }

import "Vec3"

type vec3 = Vec3.Int.t
def f (a: vec3, b: vec3) : i32 = Vec3.Int.dot (a, b)

def main (a1: i32) (a2: i32) (a3: i32) (b1: i32) (b2: i32) (b3: i32) : i32 =
  Vec3.Int.dot ((a1, a2, a2), (b1, b2, b3))
