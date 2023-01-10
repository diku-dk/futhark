-- Simplify indexing of an in-place update where we just wrote to that
-- index.
-- ==
-- input { [1,2,3] 0 42 }
-- output { 42 }
-- input { [1,2,3] 6 42 }
-- error: out of bounds
-- structure { Update 0 }

def main (xs: *[]i32) i v =
  let xs[i] = v
  in xs[i]
