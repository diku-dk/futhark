-- Enclosing parameters must not be confused in an inner parameter.
-- Based on #2615.
-- ==
-- error: val f.g

module A : {module f : (X: {type t}) -> (Y: {type u}) -> {val g : X.t -> Y.u} -> {}} = {
  module f (X: {type t}) (Y: {type u}) (_Z: {val g : Y.u -> X.t}) = {}
}
