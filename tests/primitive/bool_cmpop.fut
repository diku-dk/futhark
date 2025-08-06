-- Test comparison of boolean values.

-- ==
-- entry: lt
-- input { [false, false, true, true ] [false, true, false, true] }
-- output { [false, true, false, false] }

-- ==
-- entry: gt
-- input { [false, false, true, true ] [false, true, false, true] }
-- output { [false, false, true, false] }

-- ==
-- entry: eq
-- input { [false, false, true, true ] [false, true, false, true] }
-- output { [true, false, false, true] }

-- ==
-- entry: lte
-- input { [false, false, true, true ] [false, true, false, true] }
-- output { [true, true, false, true] }

-- ==
-- entry: gte
-- input { [false, false, true, true ] [false, true, false, true] }
-- output { [true, false, true, true] }

entry lt (x: []bool) (y: []bool) = map2 (<) x y
entry gt (x: []bool) (y: []bool) = map2 (>) x y
entry eq (x: []bool) (y: []bool) = map2 (==) x y
entry lte (x: []bool) (y: []bool) = map2 (<=) x y
entry gte (x: []bool) (y: []bool) = map2 (>=) x y
