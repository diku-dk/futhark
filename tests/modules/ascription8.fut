-- Ascription of a module containing a parametric module whose
-- parameter contains an abstract type.
-- ==
-- input {} output {0.0}

module type sobol = {
  module Reduce :
      (X : { type t
             val ne : t }) -> { val run : i32 -> X.t }
}


module Sobol: sobol = {

  module Reduce (X : { type t
                       val ne : t }) : { val run : i32 -> X.t } =
  {
    let run (N:i32) : X.t = copy X.ne
  }
}

module R = Sobol.Reduce { type t = f64
                          let ne = 0f64
                        }

let main : f64 = R.run 100000
