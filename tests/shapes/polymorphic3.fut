-- We must be able to infer size-preserving function types.

def set i v arr =
  copy arr with [i] = v

def main [n] (xs: [n]i32) : [n]i32 =
  set 0 0 xs
