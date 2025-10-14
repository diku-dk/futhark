-- Fusion must also happen to constants
-- ==
-- structure { Screma 1 }

def n = 1000 : i64
def x = map (+ 2) (map (+ 3) (iota n))

def main = x
