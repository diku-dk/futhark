-- Array of tuples polymorphism.
-- ==
-- input { 2 } output { [1,0] [1.0,0.0] [1,0] }

module pm (P: { type vector [n] 't val reverse [n] 't: vector [n] t -> vector [n] t }) = {
  let reverse_triple [n] 'a 'b (xs: (P.vector [n] (a,b,a))) =
    P.reverse xs
}

module m = pm { type vector [n] 't = [n]t let reverse 't (xs: []t) = xs[::-1] }

let main (x: i32) =
  unzip3 (m.reverse_triple (zip3 (iota x) (map r64 (iota x)) (iota x)))
