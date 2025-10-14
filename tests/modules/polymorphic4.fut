-- Array of tuples polymorphism.
-- ==
-- input { 2i64 } output { [1i64,0i64] [1.0,0.0] [1i64,0i64] }

module pm (P: {type vector [n] 't val reverse [n] 't : vector [n] t -> vector [n] t}) = {
  def reverse_triple [n] 'a 'b (xs: (P.vector [n] (a, b, a))) =
    P.reverse xs
}

module m = pm {type vector [n] 't = [n]t def reverse 't (xs: []t) = xs[::-1]}

def main (x: i64) =
  unzip3 (m.reverse_triple (zip3 (iota x) (map f64.i64 (iota x)) (iota x)))
