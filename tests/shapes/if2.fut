-- Looking at the size of an existential branch.
-- ==
-- input {  true 1i64 2i64 } output { 1i64 }
-- input { false 1i64 2i64 } output { 2i64 }

def main b n m =
  length (if b then iota n else iota m)
