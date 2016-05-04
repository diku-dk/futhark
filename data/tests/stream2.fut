-- ==
-- input {
--   5
--   [3,9,4,7,19]
-- }
-- output {
--   ( 0
--   , [9, 7, 19]
--   )
-- }
fun (int,[int]) main(int m, *[int,n] A) =
  streamSeq( fn (int,*[int]) (int chunk, int acc, *[int] C) =>
                    let W = filter( >6, C ) in
                    ( acc, W )
           , 0, A
           )


