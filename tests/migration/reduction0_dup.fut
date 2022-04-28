-- Common subexpression elimination should eliminate duplicate array reads
-- so no migration reduction is necessary.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 1
-- }

def main (arr: [3]f32) : f32 =
  arr[0] + arr[0]
