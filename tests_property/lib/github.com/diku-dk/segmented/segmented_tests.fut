-- | ignore

import "segmented"

-- ==
-- entry: test_segmented_scan
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] }
-- output { [1i64,3i64,6i64,4i64,9i64,15i64,7i64,15i64,24i64,34i64] }
-- input { [true] [1i64] }
-- output { [1i64] }
-- input { empty([0]bool) empty([0]i64) }
-- output { empty([0]i64) }

entry test_segmented_scan (flags: []bool) (as: []i64) =
  segmented_scan (+) 0 flags as

-- ==
-- entry: test_segmented_reduce
-- input { [true,false,false,true,false,false,true,false,false,false]
--         [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64,10i64] }
-- output { [6i64,15i64,34i64] }
-- input { [true] [1i64] }
-- output { [1i64] }

entry test_segmented_reduce (flags: []bool) (as: []i64) =
  segmented_reduce (+) 0 flags as

-- ==
-- entry: test_replicated_iota
-- input { [2i64,3i64,1i64] } output { [0i64,0i64,1i64,1i64,1i64,2i64] }
-- input { [3i64] } output { [0i64,0i64,0i64] }
-- input { [2i64,0i64,1i64] } output { [0i64,0i64,2i64] }
-- input { empty([0]i64) } output { empty([0]i64) }
-- input { [0i64] } output { empty([0]i64) }
-- input { [0i64,0i64] } output { empty([0]i64) }

entry test_replicated_iota (repl:[]i64) : []i64 =
  replicated_iota repl

-- ==
-- entry: test_segmented_iota
-- input { [false,false,false,true,false,false,false] }
-- output { [0i64,1i64,2i64,0i64,1i64,2i64,3i64] }
-- input { [false] } output { [0i64] }
-- input { [true] } output { [0i64] }
-- input { empty([0]bool) } output { empty([0]i64) }

entry test_segmented_iota (flags:[]bool) : []i64 =
  segmented_iota flags

-- ==
-- entry: test_expand
-- input { [2i64,3i64,1i64] }
-- output { [0i64,2i64,0i64,3i64,6i64,0i64] }

entry test_expand (arr:[]i64) : []i64 =
  expand (\ x -> x) (\x i -> x*i) arr

-- ==
-- entry: test_expand_reduce
-- input { [2i64,0i64,3i64,1i64] }
-- output { [2i64,9i64,0i64] }

entry test_expand_reduce (arr:[]i64) : []i64 =
  expand_reduce (\ x -> x) (\x i -> x*i) (+) 0 arr

-- ==
-- entry: test_expand_outer_reduce
-- input { [2i64,0i64,3i64,1i64] }
-- output { [2i64,0i64,9i64,0i64] }

entry test_expand_outer_reduce (arr:[]i64) : []i64 =
  expand_outer_reduce (\ x -> x) (\x i -> x*i) (+) 0 arr
