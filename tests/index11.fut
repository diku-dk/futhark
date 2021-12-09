-- Index projection!
-- ==
-- input { [1,2,3] [[true,false],[false,true]] }
-- output { 1 [2,3] [true,false] [[false,true]] }

def newhead = (.[0])

def newtail = (.[1:])

def main (xs: []i32) (ys: [][]bool) =
  (newhead xs, newtail xs, newhead ys, newtail ys)
