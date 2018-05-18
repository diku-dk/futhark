-- Tests of various array functions from the basis library.

import "/futlib/array"

-- ==
-- entry: test_length
-- input { empty(i32) } output { 0 }
-- input { [1,2,3] } output { 3 }

entry test_length (x: []i32) = length x

-- ==
-- entry: test_null
-- input { empty(i32) } output { true }
-- input { [1,2,3] } output { false }

entry test_null (x: []i32) = null x

-- ==
-- entry: test_head
-- input { empty(bool) } error: failed
-- input { [true,false] } output { true }

entry test_head (x: []bool) = head x

-- ==
-- entry: test_tail
-- input { empty(bool) } error: failed
-- input { [true] } output { empty(bool) }
-- input { [true,false] } output { [false] }

entry test_tail (x: []bool) = tail x

-- ==
-- entry: test_init
-- input { empty(bool) } error: failed
-- input { [true] } output { empty(bool) }
-- input { [true,false] } output { [true] }

entry test_init (x: []bool) = init x

-- ==
-- entry: test_last
-- input { empty(bool) } error: failed
-- input { [true] } output { true }
-- input { [true,false] } output { false }

entry test_last (x: []bool) = last x

-- ==
-- entry: test_take
-- input { 0 empty(bool) } output { empty(bool) }
-- input { 1 empty(bool) } error: failed
-- input { 0 [true] } output { empty(bool) }
-- input { 1 [true] } output { [true] }
-- input { 1 [true,false] } output { [true] }
-- input { 2 [true,false,true] } output { [true,false] }

entry test_take (i: i32) (x: []bool) = take i x

-- ==
-- entry: test_drop
-- input { 0 empty(bool) } output { empty(bool) }
-- input { 1 empty(bool) } error: failed
-- input { 0 [true] } output { [true] }
-- input { 1 [true] } output { empty(bool) }
-- input { 1 [true,false] } output { [false] }
-- input { 2 [true,false,true] } output { [true] }

entry test_drop (i: i32) (x: []bool) = drop i x

-- ==
-- entry: test_reverse
-- input { [[1,2],[3,4],[5,6]] } output { [[5,6],[3,4],[1,2]] }

entry test_reverse (x: [][]i32) = reverse x

-- ==
-- entry: test_update
-- input { [1,2,3] 0 4 } output { [4,2,3] }
-- input { [1,2,3] -1 4 } error: failed
-- input { [1,2,3] 3 4 } error: failed

entry test_update (xs: *[]i32) (i: i32) (x: i32) = update xs i x

-- ==
-- entry: test_or
-- input { [true, true] }
-- output { true }
-- input { [true, false] }
-- output { true }
-- input { [false, false] }
-- output { false }
-- input { empty(bool) }
-- output { false }

entry test_or (xs: []bool) = or xs

-- ==
-- entry: test_or
-- input { [true, true] }
-- output { true }
-- input { [true, false] }
-- output { false }
-- input { [false, false] }
-- output { false }
-- input { empty(bool) }
-- output { true }

entry test_and (xs: []bool) = and xs

-- ==
-- entry: test_pick
-- input { [true,false,true] [1,2,3] [4,5,6] }
-- output { [1,5,3] }

entry test_pick (flags: []bool) (xs: []i32) (ys: []i32) = pick flags xs ys

-- ==
-- entry: test_flatten
-- input { [[1,2],[3,4]] } output { [1,2,3,4] }

entry test_flatten (xs: [][]i32) = flatten xs

-- ==
-- entry: test_intersperse
-- input { 0 empty(i32) } output { empty(i32) }
-- input { 0 [1] } output { [1] }
-- input { 0 [1,2] } output { [1,0,2] }
-- input { 0 [1,2,3] } output { [1,0,2,0,3] }

entry test_intersperse (x: i32) (xs: []i32) = intersperse x xs

-- ==
-- entry: test_intercalate
-- input { empty(i32) empty([]i32) } output { empty(i32) }
-- input { [1,0] [[1,2],[3,4]] } output { [1,2,1,0,3,4] }
-- input { [1,0] [[1,2],[3,4],[5,6]] } output { [1,2,1,0,3,4,1,0,5,6] }

entry test_intercalate [m] (x: [m]i32) (xs: [][m]i32) = intercalate x xs

-- ==
-- entry: test_foldl
-- input { 10 } output { -45 }
entry test_foldl (n: i32) = foldl (-) 0 (iota n)

-- ==
-- entry: test_foldr
-- input { 10 } output { -5 }
entry test_foldr (n: i32) = foldr (-) 0 (iota n)
