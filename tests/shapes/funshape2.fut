-- ==
-- error: Causality check

def main xs = (\f' -> f' (filter (>0) xs)) (\_ -> 0)
