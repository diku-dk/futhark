-- Ascription of a module containing a parametric module whose
-- parameter contains an abstract type.
-- ==
-- input {} output {0.0f32}
-- warning: ^$

module type sobol = {
  module Reduce: (X: {
    type t
    val ne : t
  }) ->
     {val run : i32 -> X.t}
}

module Sobol : sobol = {
  module Reduce
    (X: {
      type t
      val ne : t
    })
    : {val run : i32 -> X.t} = {
    def run (_N: i32) : X.t = copy X.ne
  }
}

module R = Sobol.Reduce {
  type t = f32
  def ne = 0f32
}

def main : f32 = R.run 100000
