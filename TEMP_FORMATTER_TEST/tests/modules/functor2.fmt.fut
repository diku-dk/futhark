-- Referring to a parameter-defined type in a functor return signature.
-- ==
-- input { 2 } output { 4 }
module F (P: {type t val f: t -> t}): {type t = P.t val f2: t -> t} = {
  type t = P.t
  
  def f2 (x: t): t = P.f (P.f x)
}

module F' = F {{type t = i32 def f (x: i32): i32 = x + 1}}

def main (x: i32): F'.t = F'.f2 x