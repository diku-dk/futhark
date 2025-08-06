-- Higher-order module specs with abstract types.
-- ==
-- input { 1 } output { 4 }

module type repeater = (P: {type t val f : t -> t}) -> {val g : P.t -> P.t}

module twice_ (P: {type t val f : t -> t}) = {
  def g (x: P.t) = P.f (P.f x)
}

module twice = twice_: repeater

module twice_mult = twice {type t = i32 def f (x: t) = x * 2}

def main (x: i32) = twice_mult.g x
