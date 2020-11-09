-- Polymorphic function in module parameter.
-- ==
-- input { [1,2] [true,false] }
-- output { [1,2,1,2] [true,false,true,false] [2,1] [false,true] }

module pm (P: { val frob 'a [n]: [n]a -> []a }) = {
  let frob_two 'a 'b (xs: []a) (ys: []b) = (P.frob xs, P.frob ys)
}

module double = pm { let frob 'a (xs: []a) = concat xs xs }
module reverse = pm { let frob 'a (xs: []a) = xs[::-1] }

let main (xs: []i32) (ys: []bool) =
  let (a,b) = double.frob_two xs ys
  let (c,d) = reverse.frob_two xs ys
  in (a,b,c,d)
