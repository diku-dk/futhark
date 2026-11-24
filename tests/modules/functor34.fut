-- A functor parameter may refer to an abstract type of the enclosing
-- module type.  Based on #2615.

module A : {type s module f : {type t = s} -> {}} = {
  type s = i32
  module f (_M: {type t = s}) = {}
}
