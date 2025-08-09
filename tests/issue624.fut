-- The problem was incorrect type substitution in the monomorphiser
-- which removed a uniqueness attribute.

module type m = {
  type^ t
  val r : *t -> *t
}

module m : m = {
  type^ t = []f32
  def r (t: *t) : *t = t
}

entry r (t: *m.t) : *m.t = m.r t
