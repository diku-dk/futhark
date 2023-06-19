-- ==
-- input { [1,2,3] [4,5,6] } output { [5, 6, 7] [7, 9, 11] }
-- structure gpu { SegMap/Apply 2 }

#[noinline]
def f y x = x + y + 2i32

def main xs ys = (map (f 2) xs, map2 f xs ys)
