-- Matrix multiplication written in a functional style.
-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }
-- structure { Map 2 Map/Map/Redomap 1 }

fun [n][p]int main([n][m]int x, [m][p]int y) =
  map(fn [p]int ([m]int xr) =>
        map(fn int ([m]int yc) =>
              reduce(+, 0, zipWith(*, xr, yc)),
            transpose(y)),
      x)
