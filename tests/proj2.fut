-- Can we map a record projection?
-- ==
-- input { [1,2] [3,4] }
-- output { [1,2] }

def main (xs: []i32) (ys: []i32) : []i32 =
  map (.x) (map2 (\x y -> {x, y}) xs ys)
