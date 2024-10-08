-- Multiple includes of the same thing is harmless, and should work.
-- ==
-- input { 1 } output { 2 3 }
module type has_a =
{
  type a
  val f: a -> a
}

module pm (M: has_a): {
  type a = (M.a, M.a)
  include has_awith a  = (M.a, M.a)
  include has_awith a  = (M.a, M.a)
} = {
  type a = (M.a, M.a)
  
  def f (x, y) = (M.f x, M.f (M.f y))
}

module M_a: has_awith a  = i32 = {
  type a = i32
  
  def f = (+ 1)
}

module M_a_a = pm M_a

def main (x: i32) =
  M_a_a.f (x, x)