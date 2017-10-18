-- Complex indexing into reshape, replicate and iota should be simplified away.
-- ==
-- input { 2 } output { 1 }
-- input { 10 } output { 3 }
-- structure { Iota 0 Replicate 0 Reshape 0 }

let main(x: i32) =
  let a = iota x
  let b = replicate x a
  let c = reshape (x*x) b
  in c[3]
