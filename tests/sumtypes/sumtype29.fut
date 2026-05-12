-- Optimise representation where the components must be flattened out
-- first.

type twod = {x: f32, y: f32}
type threed = {x: f32, y: f32, z: f32}

type point = #twod twod | #threed threed

def main (p: point) : point =
  match p
  case #twod ds -> #threed {x = ds.x, y = ds.y, z = 0}
  case #threed {x, y, z = _} -> #twod {x, y}
