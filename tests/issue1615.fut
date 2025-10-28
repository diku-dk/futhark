-- ==
-- structure { Update 1 }

def main (A: *[3]i32) : *[3]i32 =
  let x = (id A)[2]
  in A with [1] = x
