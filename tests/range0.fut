-- Basic ranges.
-- ==
-- entry: test0
-- input { 0 5 } output { [0i32, 1i32, 2i32, 3i32, 4i32, 5i32] }
-- input { 1 5 } output { [1i32, 2i32, 3i32, 4i32, 5i32] }
-- input { 5 1 } error: 5...1
-- input { 5 0 } error: 5...0

-- ==
-- entry: test1
-- input { 0 5 } output { [0i32, 1i32, 2i32, 3i32, 4i32] }
-- input { 1 5 } output { [1i32, 2i32, 3i32, 4i32] }
-- input { 5 1 } error: 5..<1
-- input { 5 0 } error: 5..<0

-- ==
-- entry: test2
-- input { 0 5 } error: 0..>5
-- input { 1 5 } error: 1..>5
-- input { 5 1 } output { [5i32, 4i32, 3i32, 2i32] }
-- input { 5 0 } output { [5i32, 4i32, 3i32, 2i32, 1i32] }

-- ==
-- entry: test3
-- input { 0 1 5 } output { [0i32, 1i32, 2i32, 3i32, 4i32, 5i32] }
-- input { 1 0 5 } error: 1..0...5
-- input { 5 4 1 } output { [5i32, 4i32, 3i32, 2i32, 1i32] }
-- input { 5 0 0 } output { [5i32, 0i32] }
-- input { 0 2 5 } output { [0i32, 2i32, 4i32] }
-- input { 1 1 5 } error: 1..1...5
-- input { 1 0 1 } output { [1i32] }
-- input { 1 2 1 } output { [1i32] }

-- ==
-- entry: test4
-- input { 0 1 5 } output { [0i32, 1i32, 2i32, 3i32, 4i32] }
-- input { 1 0 5 } error: 1..0..<5
-- input { 5 4 1 } error: 5..4..<1
-- input { 5 0 0 } error: 5..0..<0
-- input { 0 2 5 } output { [0i32, 2i32, 4i32] }

-- ==
-- entry: test5
-- input { 0 1 5 } error: 0..1..>5
-- input { 1 0 5 } error: 1..0..>5
-- input { 5 4 1 } output { [5i32, 4i32, 3i32, 2i32] }
-- input { 5 0 0 } output { [5i32] }
-- input { 0 2 5 } error: 0..2..>5

entry test0 (start: i32) (end: i32) = start...end
entry test1 (start: i32) (end: i32) = start..<end
entry test2 (start: i32) (end: i32) = start..>end
entry test3 (start: i32) (step: i32) (end: i32) = start..step...end
entry test4 (start: i32) (step: i32) (end: i32) = start..step..<end
entry test5 (start: i32) (step: i32) (end: i32) = start..step..>end
