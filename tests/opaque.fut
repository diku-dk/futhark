-- Test that 'opaque' prevents constant-folding.
-- ==
-- input {} output {4}
-- structure { BinOp 1 Opaque 1 }

def main = opaque 2 + 2
