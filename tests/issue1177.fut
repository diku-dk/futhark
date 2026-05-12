-- We messed up because this loop is existential at *two* levels (the
-- outer dimension and the nested dimension).  Although the mess was
-- in the let-binding, and due to how we constructed patterns for
-- statements.

def main n =
  loop acc = [([1], 1)]
  for i < n do
    replicate i (replicate (n - i) 1, i)
