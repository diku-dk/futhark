-- Applying an ascribed curried functor whose second parameter refers
-- to the first.  Based on #2615.
-- ==
-- input { } output { 3i32 }

module A : {module f : (X: {type t val x : t}) -> {val y : X.t} -> {val z : X.t}} = {
  module f (X: {type t val x : t}) (Y: {val y : X.t}) = {def z = Y.y}
}

module B = A.f {type t = i32 def x = 1i32} {def y = 2i32}

def main = B.z + 1
