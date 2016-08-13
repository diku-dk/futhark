-- ==
-- input {
--   10 21 21
--   19 12 5
-- }
-- output {
--   547
-- }

include Vec3

type vec3 = Vec3.Int.t
fun int f(vec3 a, vec3 b) = Vec3.Int.dot(a , b)
fun int main(int a1, int a2, int a3, int b1, int b2, int b3) =
  Vec3.Int.dot((a1,a2,a2) , (b1,b2,b3))

