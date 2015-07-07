-- Test map in which the map function consumes its argument.
-- ==
-- input {
--   [[1,2,3], [4,5,6]]
--   1
--   1337
-- }
-- output {
--   [[1,1337,3], [4,1337,6]]
-- }
fun *[[int]] main(*[[int]] a, int i, int x) =
  map(fn *[int] (*[int] r) =>
        let r[i] = x in r,
      a)
