-- http://rosettacode.org/wiki/Arithmetic/Complex
--
-- We implement a complex number as a pair of floats.  This would be
-- nicer with operator overloading.
--
-- input { 0 1.0 1.0 3.14159 1.2 }
-- output { 3.14159 2.2 }
-- input { 1 1.0 1.0 3.14159 1.2 }
-- output { 1.94159f64 4.34159f64 }
-- input { 2 1.0 1.0 3.14159 1.2 }
-- output { 0.5f64 -0.5f64 }
-- input { 3 1.0 1.0 3.14159 1.2 }
-- output { -1.0f64 -1.0f64 }
-- input { 4 1.0 1.0 3.14159 1.2 }
-- output { 1.0f64 -1.0f64 }


type complex = (f64,f64)

let complexAdd((a,b): complex) ((c,d): complex): complex =
  (a + c,
   b + d)

let complexMult((a,b): complex) ((c,d): complex): complex =
 (a*c - b * d,
  a*d + b * c)

let complexInv((r,i): complex): complex =
  let denom = r*r + i * i
  in (r / denom,
      -i / denom)

let complexNeg((r,i): complex): complex =
  (-r, -i)

let complexConj((r,i): complex): complex =
  (r, -i)

let main (o: i32) (a: complex) (b: complex): complex =
  if      o == 0 then complexAdd a b
  else if o == 1 then complexMult a b
  else if o == 2 then complexInv a
  else if o == 3 then complexNeg a
  else                complexConj a
