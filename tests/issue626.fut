-- ==
-- error: consuming

module type m1 = {
  type t
  val get : *t -> f32 -> f32
}

module m1 : m1 = {
  type t = [1]f32
  def get (t: *t) (v: f32) : f32 = t[0]
}

entry read (t: *[1]m1.t) : []f32 = map2 m1.get t [0]
