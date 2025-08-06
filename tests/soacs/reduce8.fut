-- #1800
-- ==
-- input { 100i64 } auto output
-- compiled input { 100000i64 } auto output

def main n = i64.sum (map (\i -> loop i = i + 1 while i < 1000 do i * 3) (iota n))
