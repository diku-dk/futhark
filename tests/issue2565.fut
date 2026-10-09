-- A consumed parameter may only be short-circuited into memory that is
-- allocated in the function, and no larger than the parameter.

-- The destination is another parameter.
-- ==
-- entry: into_param
-- input { [1,2,3] [[0,0,0],[0,0,0],[0,0,0]] }
-- output { [[1,2,3],[0,0,0],[0,0,0]] }

entry into_param (zs: *[3]i32) (xss: *[3][3]i32) = let xss[0] = zs in xss

-- The destination is larger than the parameter.
-- ==
-- entry: into_larger
-- input { [1,2,3] [4,5,6,7,8] }
-- output { [1,2,3,4,5,6,7,8] }

entry into_larger [n] (zs: *[3]i32) (ys: [n]i32) = zs ++ ys
