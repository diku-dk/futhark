-- ==
-- entry: rev
-- input { [1,2,3] [3,2,1] }
-- output { [6,4,2] }

entry rev = vjp (map (*2i32))
