import "futlib/monoid"
import "futlib/segmented"

-- ==
-- entry: test_segmented_scan
-- input { [true,false,false,true,false,false,true,false,false,false] [1,2,3,4,5,6,7,8,9,10] }
-- output { [1,3,6,4,9,15,7,15,24,34] }

module i32plus = {
  type t = i32
  let ne = 0
  fun op (x: i32) (y: i32) = x + y
}

module segprefixsum = segmented_scan(i32plus)

entry test_segmented_scan (flags: []bool) (as: []i32) =
  segprefixsum.segmented_scan flags as
