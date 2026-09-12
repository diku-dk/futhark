-- ==
-- input { [1,2,3] } output { [0,2,3] }

def f : []i32 -> *[]i32 = id >-> copy

entry main xs = f xs with [0] = 0
