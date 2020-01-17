-- Polymorphic function using polymorphic type in parametric module.
-- ==
-- input { 2 3 } output { [1,0] [2.0,1.0,0.0] }

module pm (P: { type^ vector 't val reverse 't: vector t -> vector t }) = {
  let reverse_pair 'a 'b ((xs,ys): (P.vector a, P.vector b)) =
    (P.reverse xs, P.reverse ys)
}

module m = pm { type^ vector 't = []t let reverse 't (xs: []t) = xs[::-1] }

let main (x: i32) (y: i32) = m.reverse_pair (iota x, map r64 (iota y))
