-- Complex indexing into reshape, replicate and iota should be simplified away.
-- ==
-- input { 2i64 } output { 1i64 }
-- input { 10i64 } output { 3i64 }
-- structure { Iota 0 Replicate 0 Reshape 0 }

def main (x: i64) =
  let a = iota x
  let b = replicate x a
  let c = flatten b
  in c[3]
