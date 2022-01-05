-- Polymorphic infix operators ought to work.
-- ==
-- input { [1,2,3] [4,5,6] [true] [false] }
-- output { [1,2,3,4,5,6] [true,false] }

def (++) 't (xs: []t) (ys: []t) = concat xs ys

def main (xs: []i32) (ys: []i32) (as: []bool) (bs: []bool) =
  (xs ++ ys, as ++ bs)
