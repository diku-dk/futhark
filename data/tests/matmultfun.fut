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

fun [[int,p],n] main([[int,m],n] x, [[int,p],m] y) =
  map(fn [int,p] ([int,m] xr) =>
        map(fn int ([int,m] yc) =>
              reduce(+, 0, zipWith(*, xr, yc)),
            transpose(y)),
      x)
