-- A functor whose parameter is itself a functor.
-- ==
-- input { 1 } output { 3 4 7 }
module F = \(P_f: (P_f_a:) {type a val f: a -> a} -> {val f: P_f_a.a -> P_f_a.a}) ->
\(P_x: {type a val f: a -> a}) ->
{type a = P_x.a open {P_f P_x}}

module twice = \(twice_P: {type a val f: a -> a}) ->
{
  def f (x: twice_P.a) = twice_P.f (twice_P.f x)
}

module thrice = \(thrice_P: {type a val f: a -> a}) ->
{
  def f (x: thrice_P.a) = thrice_P.f (thrice_P.f (thrice_P.f x))
}

module add_one = {type a = i32 def f (x: i32) = x + 1}

module F_2 = F twice add_one

module F_3 = F thrice add_one

module F_6 = F twice F_3

def main (x: i32) = (F_2.f x, F_3.f x, F_6.f x)