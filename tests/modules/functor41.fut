-- A module function applied inside the body of a partially applied
-- curried module function, whose own body applies another module
-- function to one of its parameter's components.  Defunctorisation
-- used to leave the parameter's abstract type unresolved.
--
-- Based on #2616.
-- ==
-- input { [3f32, 1f32, 2f32] } output { [3f32, 1f32, 2f32] }

module type num = {type t}
module type fl = {type t type int_t val to_bits : t -> int_t}

module type rf = {
  include fl
  module int_t: num with t = int_t
}

module mk (f: fl) (b: num with t = f.int_t) = {
  type t = f.t
  type int_t = f.int_t
  def to_bits = f.to_bits
  module int_t = b
}

module r32 = mk {type t = f32 type int_t = u32 def to_bits = f32.to_bits} {type t = u32}

module radix_sort (T: {}) (U: {}) = {
  module integral (bytes: num) = {
    def sort_on [n] 'a (f: a -> bytes.t) (ys: [n]a) = let _ = map f ys in ys
  }

  module float (f: rf) = {
    module sort = integral f.int_t
    def sort_on [n] (as: [n]f.t) : [n]f.t = sort.sort_on f.to_bits as
  }

  module f32 = float r32
}

module R32 = radix_sort {} {}
entry main (xs: []f32) = R32.f32.sort_on xs
