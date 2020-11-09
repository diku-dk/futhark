-- Range exclusion
-- ==
-- entry: test0
-- input { 10 20 } error: 10..10...20
-- input { 1 2 } error: 1..1...2
-- input { 20 10 } error: 20..20...10
-- input { 20 -10 } error: 20..20...-10
-- input { 5 0 } error: 5..5...0
-- input { 5 -1 } error: 5..5...-1

-- ==
-- entry: test1
-- input { 10 20 } error: 10..10..<20
-- input { 1 2 } error: 1..1..<2
-- input { 20 10 } error: 20..20..<10
-- input { 20 -10 } error: 20..20..<-10
-- input { 5 0 } error: 5..5..<0
-- input { 5 -1 } error: 5..5..<-1

-- ==
-- entry: test2
-- input { 10 20 } error: 10..10..>20
-- input { 1 2 } error: 1..1..>2
-- input { 20 10 } error: 20..20..>10
-- input { 20 -10 } error: 20..20..>-10
-- input { 5 0 } error: 5..5..>0
-- input { 5 -1 } error: 5..5..>-1

entry test0 (start: i32) (end: i32) = start..start...end
entry test1 (start: i32) (end: i32) = start..start..<end
entry test2 (start: i32) (end: i32) = start..start..>end
