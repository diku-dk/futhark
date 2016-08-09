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
fun int main(vec3 a, vec3 b) = Vec3.Int.dot(a , b)

