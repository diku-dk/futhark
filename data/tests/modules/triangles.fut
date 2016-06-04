-- ==
-- input {
--   10 21 21 19 12 5
-- }
-- output {
--   547
-- }


include Vec3

fun int main(int a, int b, int c, int d, int e, int f) =
  let vecA = (a,b,c) in
  let vecB = (d,e,f) in
  Vec3.Int.dot(vecA, vecB)

