-- ==
-- input { [true, false] }
-- output { [1f32, f32.nan] }

def main = map (\x -> if x then 1 else f32.nan)
