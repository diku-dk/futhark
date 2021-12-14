-- Does the f16.acosh function work?
-- ==
-- input { [1f16, 0.5403023f16, 3.14f16] }
-- output { [0f16,  f16.nan, 1.810991348900196f16 ] }

def main = map f16.acosh
