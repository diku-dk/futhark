-- A curried functor parameter must not demand more than the ascribed
-- parameter provides.  Based on #2615.
-- ==
-- error: type f.t = i32

module A : {module f : (X: {type t}) -> {type t = X.t} -> {}} = {
  module f (X: {type t}) (_M: {type t = i32}) = {}
}
