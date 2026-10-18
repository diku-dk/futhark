-- A parametric module type used for two ascriptions in a row.  The
-- type parameters in the result of the parametric module type must be
-- freshened for each use, lest the solver see the same name as both a
-- rigid and a nonrigid type parameter.

module type ft = (X: {type t}) -> {val f 'a : X.t -> a -> a}

module g : ft = \(X: {type t}) -> {def f 'a (_: X.t) (x: a) = x}

module h : ft = g
