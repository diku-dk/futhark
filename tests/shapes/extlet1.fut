-- A type becomes existential because a name goes out of scope,
-- trickier.
-- ==
-- input { 1i64 } output { 2i64 }

def main n =
  length (let m = n + 1 in iota m)
