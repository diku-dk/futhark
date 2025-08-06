-- Record updates should respect uniqueness and aliases.
-- ==
-- error: "s", which is not consumable

type^ state = {size: i64, world: []i32}

def init (size: i64) : state = {size, world = replicate size 0}

def main (size: i64) (s: state) : *[]i32 =
  (init size with world = s.world).world
