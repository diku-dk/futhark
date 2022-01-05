-- Does the f16.atanh function work?
-- ==
-- input { [0f16, 0.5f16, 1f16, -1f16] }
-- output { [0f16, 0.5493061443340548f16, f16.inf, -f16.inf] }

def main = map f16.atanh
