-- Record updates should respect uniqueness and aliases.
-- ==
-- error: "s"

type^ state = { size: i64, world: []i32 }

let init (size: i64): state = {size, world = replicate size 0}

let main (size: i64) (s: state) : *[]i32 =
  (init size with world = s.world).world
