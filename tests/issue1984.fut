-- #19
-- ==
-- input { true [1,2,3] } output { [1,2,3] }
-- input { false [1,2,3] } output { [3,2,1] }

def main b (xs: []i32) = if b then xs else reverse xs
