-- Not safe to fuse these two replicates, at least not without being
-- more clever about it than we currently are.
-- ==
-- structure {Replicate 2}

def main n m =
  replicate n (replicate n 0) ++ replicate m (replicate n 0)
