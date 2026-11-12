-- Division by zero, and in a parallel context at that!
-- ==
-- tags { no_webgpu }
-- input { [0] } error:

def main (xs: []i32) = map (2 /) xs
