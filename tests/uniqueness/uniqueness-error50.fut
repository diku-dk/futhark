-- The result of a loop can have aliases.
-- ==
-- error: "chunk"

def vecadd [m] (xs: [m]i32) (ys: [m]i32) : [m]i32 =
  ys

def main [m] chunk_sz (chunk: [chunk_sz][m]i32) : *[m]i32 =
  loop acc = replicate m 0 for i < chunk_sz do vecadd acc chunk[i]
