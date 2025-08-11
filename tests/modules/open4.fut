-- Opening a module defining a type is not the same as defining a
-- type.  Confused the type checker at one point.
-- ==
-- input { 2 } output { 2 }

module pm (P: {type t}) (X: {}) = {
  open P
  def id_t (x: t) : t = x
}

module p_is_i32 = {type t = i32}

module pm_i32 = pm p_is_i32 {}

def main (x: i32) : i32 = pm_i32.id_t x
