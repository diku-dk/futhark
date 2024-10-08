module type rng_engine =
{
  module int: {type t val to_i64: t -> i64}
  val min: int.t
}

module pcg32: rng_enginewith int.t  = u32 = {
  module int = {type t = u32 def to_i64 = u32.to_i64}: {type t = u32 val to_i64: t -> i64}
  
  def min = 0u32
}

module uniform_int_distribution (E: {
    module int: {type t val to_i64: t -> i64}
    val min: int.t
  }) = {
  def v = E.int.to_i64 E.min
}

module dist = uniform_int_distribution pcg32

def main = dist.v