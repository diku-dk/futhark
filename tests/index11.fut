-- Index projection!
-- ==
-- input { [1,2,3] [[true,false],[false,true]] }
-- output { 1 [2,3] [true,false] [[false,true]] }

let newhead = (.[0])

let newtail = (.[1:])

let main (xs: []i32) (ys: [][]bool) =
  (newhead xs, newtail xs, newhead ys, newtail ys)
