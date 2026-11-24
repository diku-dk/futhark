-- The concrete functor parameter may be more general than the
-- ascribed one.  Based on #2615.
-- ==
-- input { } output { 1i32 }

module A : {module f : (X: {type t = i32 val x : i32}) -> {val y : X.t}} = {
  module f (X: {type t val x : t}) = {def y = X.x}
}

module B = A.f {type t = i32 def x = 1i32}

def main = B.y
