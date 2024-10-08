module type newreal = {
  type t
  val f32 : f32 -> t
}

module type newint = {
  type t
  val f32 : f32 -> t
}

module newf32 : newreal with t = f32 = {
  type t = f32
  def f32 = f32.f32
}

module newi32 : newint with t = i32 = {
  type t = i32
  def f32 = i32.f32
}

module type mixture = {
    module V : newreal
    module I : newint
}

module em (P: mixture) = {
  module mixture = P
}

module k_means_mixture (P: mixture) = {
    module V = P.V
    module I = P.I
}

module foo = {
  module V = newf32
  module I = newi32
}

module bar = k_means_mixture foo
module baz = bar : mixture
module k_means_em = em baz

def main (x: k_means_em.mixture.V.t) = x
