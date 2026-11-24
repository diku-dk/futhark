-- Innermost functor parameter refers to both enclosing parameters.
-- Based on #2615.

module A : {
  module f : (X: {type t}) -> (Y: {type u}) -> {val g : X.t -> Y.u} -> {val h : X.t -> Y.u}
} = {
  module f (X: {type t}) (Y: {type u}) (Z: {val g : X.t -> Y.u}) = {def h = Z.g}
}
