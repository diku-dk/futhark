-- Removing a copy of a parameter must not make a fresh result alias
-- the parameter.

-- ==
-- entry: main
-- input { [1,2,3] } output { [1,2,3] }

entry main (ys: [3]i32) = loop acc = copy ys for i < 2 do copy ys

-- ==
-- entry: inlined
-- input { [1,2,3] } output { [0,2,3] [1,2,3] }

def f (ys: [3]i32) : *[3]i32 = loop _acc = copy ys for _i < 2 do copy ys

entry inlined (ys: [3]i32) = let r = f ys in (r with [0] = 0, ys)
