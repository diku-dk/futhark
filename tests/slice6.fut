-- Really careful when copying these slices!
-- ==
-- input { [1,2,3,4,5,6,7,8,9,10,11,12,13] [13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1] }
-- output { [1, 12, 3, 10, 5, 8, 7, 6, 9, 4, 11, 12, 13] }

def main (xs: *[]i32) (ys: []i32) =
  xs with [1:10:2] = ys[1:10:2]
