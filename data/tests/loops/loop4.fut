-- Nasty loop whose size cannot be predicted in advance.
-- ==
-- input {
--   [1,2,3]
--   4
-- }
-- output {
--   [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
-- }

fun []int main([]int xs, int n) =
  loop (xs) = for i < n do
    concat(xs,xs)
  in xs
