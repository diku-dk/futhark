-- A functor parameter referring to an abstract type of the enclosing
-- module type must agree with its definition.  Based on #2615.
-- ==
-- error: type f.t = f32

module A : {type s module f : {type t = s} -> {}} = {
  type s = i32
  module f (_M: {type t = f32}) = {}
}
