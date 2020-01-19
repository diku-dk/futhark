-- Inference of record in lambda.
-- ==
-- error: Full type of

type octnode = {body: i32}

let f (octree: []octnode) (i: i32) =
  map (\n -> if n.body != i then n
             else n with body = 0)
      octree
