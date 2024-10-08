-- Inference of record in lambda.
-- ==
-- error: Full type of
type octnode = {body: i32}

def f (octree: []octnode) (i: i32) =
  map
    (
      \n ->
        if n.body != i then
        n
        else
        0 n with body =
    ) octree