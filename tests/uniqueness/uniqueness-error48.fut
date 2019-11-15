-- Record updates should respect uniqueness and aliases.
-- ==
-- error: `s`

type^ state = { size: i32, world: []i32 }

let init (size: i32): state = {size, world = replicate size 0}

let main (size: i32) (s: state) : *[]i32 =
  (init size with world = s.world).world
