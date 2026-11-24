--  Based on #2615.

module A
  : {
      module f: (X: {type t}) -> {type t = X.t} -> {}
    } = {
  module f (X: {type t}) (_M: {type t = X.t}) = {}
}
