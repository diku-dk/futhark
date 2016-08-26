-- ==
-- input {
--   5
--   [3,9,4,7,19]
-- }
-- output {
--   [9, 7, 19]
--
-- }
fun main(m: int, a: *[n]int): []int =
  streamMap( fn (chunk: int, c: *[]int): []int  =>
                    let w = filter( >6, c ) in
                    w
        , a
        )


