-- ==
-- input {
--   5
--   [3,9,4,7,19]
-- }
-- output {
--   0
--   [9, 7, 19]
-- }
fun (int,[]int) main(int m, *[n]int a) =
  streamSeq( fn (int,*[]int) (int chunk, int acc, *[]int c) =>
                    let w = filter( >6, c ) in
                    ( acc, w )
           , 0, a
           )


