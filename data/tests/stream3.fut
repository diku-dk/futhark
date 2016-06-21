-- ==
-- input {
--   5
--   [3,9,4,7,19]
-- }
-- output {
--   [9, 7, 19]
--
-- }
fun []int main(int m, *[n]int a) =
  streamMap( fn []int (int chunk, *[]int c) =>
                    let w = filter( >6, c ) in
                    w
        , a
        )


