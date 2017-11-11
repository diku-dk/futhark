-- Array of tuples polymorphism.
-- ==
-- input { 2 } output { [1,0] [1.0,0.0] [1,0] }

module pm (P: { type vector 't val reverse 't: vector t -> vector t }) = {
  let reverse_triple 'a 'b (xs: (P.vector (a,b,a))) =
    P.reverse xs
}

module m = pm { type vector 't = []t let reverse 't (xs: []t) = xs[::-1] }

let main (x: i32) =
  unzip (m.reverse_triple (zip (iota x) (map r64 (iota x)) (iota x)))
