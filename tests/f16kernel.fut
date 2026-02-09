-- Can we correctly have a free variable of type f16 inside a parallel
-- construct?
-- ==
-- input { [1f16,2f16] 3f16} auto output

def main (xs: []f16) (y: f16) = map (+ y) xs

-- ==
-- entry: sum
-- input { [1f16, 2f16, 3f16] } output { 6f16 }

entry sum = f16.sum
