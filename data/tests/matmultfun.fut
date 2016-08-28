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

fun main(x: [n][m]int, y: [m][p]int): [n][p]int =
  map (fn (xr) =>
        map (fn (yc) =>
              reduce (+) 0 (zipWith (*) xr yc)) (
            transpose(y))) x
