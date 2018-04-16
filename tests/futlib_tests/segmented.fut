import "/futlib/segmented"

-- ==
-- entry: test_segmented_scan
-- input { [true,false,false,true,false,false,true,false,false,false] [1,2,3,4,5,6,7,8,9,10] }
-- output { [1,3,6,4,9,15,7,15,24,34] }

entry test_segmented_scan (flags: []bool) (as: []i32) =
  segmented_scan (+) 0 flags as
