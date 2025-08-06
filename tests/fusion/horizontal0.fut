-- No fusion of this, because the arrays are independent.
-- ==
-- structure { Screma 2 }

def main [n] (x: [n]i32) (y: [n]i32) = (map (+ 1) x, map (+ 2) y)
