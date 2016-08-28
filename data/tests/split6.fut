-- Split a row of an array.
--
-- Now we are thinking with portals (and the code generator messed
-- this up at one point).
--
-- The reason I return the sums and not the arrays is that the code
-- generator gets of too easy if it can just directly
-- allocate/manifest the arrays for the function return.  This way is
-- more likely to trigger bugs.
-- ==
-- input {
--   1
--   2
--   [[4,3,1],[3,2,8],[42,1337,1],[1,0,42]]
-- }
-- output {
--   42
--   1338
-- }
fun main(n: int, i: int, a: [][]int): (int,int) =
  let (a,b) = split (n) a[i] in
  (reduce((+), 0, a), reduce((+), 0, b))
