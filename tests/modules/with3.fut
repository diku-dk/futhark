-- Keeping track of when names have to be qualified.
-- ==
-- input { 1 } output { 3 }

module type has_t = {type t}
module type has_x = {type t val x : t}

module pm (R: has_t) (V: has_x with t = R.t) = {
  def x = V.x
}

module m = pm {type t = i32} {type t = i32 def x = 2}

def main (y: i32) = y + m.x
