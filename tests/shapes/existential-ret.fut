-- Two distinct applications of the function should not interfere with
-- each other.
-- ==
-- input { [1,2,3] [4,-2,1] }
-- output { 3i64 2i64 }

def f xs = length (filter (> 0) xs)

def main xs ys = (f xs, f ys)
