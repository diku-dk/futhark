-- Entry point result should not be copied.
-- ==
-- structure gpu-mem { Copy 0 }
-- structure seq-mem { Copy 0 }

def main b n =
  if b then iota n else replicate n 0
