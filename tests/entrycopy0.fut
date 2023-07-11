-- Entry point result should not be copied.
-- ==
-- structure gpu-mem { Replicate 1 }
-- structure seq-mem { Replicate 1 }

def main b n =
  if b then iota n else replicate n 0
