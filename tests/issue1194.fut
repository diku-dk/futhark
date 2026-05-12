-- ==
-- input { 8i64 } output { 511i64 }

def main (n: i64) =
  loop i = 0
  for d in n..n - 1...0 do
    i + (1 << n - d)
