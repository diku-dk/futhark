-- ==
-- input {
--   [ [1,2,3], [5,6,7], [8,9,10] ]
-- }
-- output {
--   [ 6, 18, 27 ]
-- }
fun [int] main ([[int]] xss) =
  map (fn int ([int] xs ) =>
         reduce(+, 0, xs)
      , xss)
