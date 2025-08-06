-- ==
-- error: type vector

module pm (P: {type vector [n] 't val reverse [n] 't : vector [n] t -> vector [n] t}) = {
  def reverse_triple [n] 'a 'b (xs: (P.vector [n] (a, b, a))) =
    P.reverse xs
}

module m = pm {type vector 't = [2]t def reverse 't (xs: []t) = xs[::-1]}
