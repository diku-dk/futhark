-- Can we map a deeper tuple projection?
-- ==
-- input { [1,2] [3,4] }
-- output { [1,2] }

let main (xs: []i32) (ys: []i32): []i32 =
  map (.1.1) (map2 (\x y -> ((x,x), y)) xs ys)
