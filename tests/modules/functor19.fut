-- Using a curried module twice should work, and notably not result in
-- anything being defined twice.
-- ==
-- input {} output {7}

module pm (A: {val x: i32}) (B: {val y: i32}) = {
  let z = A.x + B.y
}

module cm = pm { let x = 2 }

module m1 = cm { let y = 1 }
module m2 = cm { let y = 2 }

let main = m1.z + m2.z
