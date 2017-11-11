-- Can we map a tuple projection?
--
-- ==
-- compiled input { [1,2,3] [4,5,6] }
-- output { [4,5,6] }

let main (xs: []i32) (ys: []i32): []i32 =
  let zs = zip xs ys
  in map #2 zs
