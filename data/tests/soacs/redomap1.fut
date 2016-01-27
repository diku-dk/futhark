-- ==
-- input {
--   [1,2,3,4,5,6,7,8]
-- }
-- output {
--   40320.000000
-- }
fun real main([int] a) =
    redomap(fn real (real x, real y) =>
              x*y,
            fn real (real x, int y) =>
              x*real(y),
            1.0,
            a)
