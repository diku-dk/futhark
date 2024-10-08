module type number = {type t val i32: i32 -> t}

module has_number: numberwith t  = i32 = {type t = i32 def i32 (x: i32) = x}

module type optimizable =
{
  module loss: number
}

module stochastic_gradient_descent (optable: optimizable) = {
  module loss = optable.loss
}

module logistic_regression (dummy: {}): optimizable = {
  module loss = has_number
}

module logreg_m = logistic_regression {}

module sgd = stochastic_gradient_descent logreg_m

def main (x: i32) = sgd.loss.i32 x