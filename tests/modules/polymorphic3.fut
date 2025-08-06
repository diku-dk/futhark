-- Polymorphic function using polymorphic type in parametric module.
-- ==
-- input { 2 3 } output { [1i64,0i64] [2.0,1.0,0.0] }

module pm (P: {type~ vector 't val reverse 't : vector t -> vector t}) = {
  def reverse_pair 'a 'b ((xs, ys): (P.vector a, P.vector b)) =
    (P.reverse xs, P.reverse ys)
}

module m = pm {type~ vector 't = ?[k].[k]t def reverse 't (xs: []t) = xs[::-1]}

def main (x: i32) (y: i32) =
  m.reverse_pair ( iota (i64.i32 x)
                 , map f64.i64 (iota (i64.i32 y))
                 )
