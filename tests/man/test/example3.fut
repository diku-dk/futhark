-- ==
-- random input { [100]i32 [100]i32 } auto output
-- random input { [1000]i32 [1000]i32 } auto output

def main xs ys = i32.product (map2 (*) xs ys)
