-- Inference of record in lambda.
-- ==

type octnode = {body: i32}

entry f (octree: []octnode) (i: i32) =
  map (\n -> if n.body != i then n
             else n with body = 0)
      octree
