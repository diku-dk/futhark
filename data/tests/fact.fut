-- ==
-- input {
--   10
-- }
-- output {
--   3628800
-- }
fun int fact(int n) =
  if n == 0 then 1
            else n * fact(n-1)

fun int main(int n) =
  fact(n)
