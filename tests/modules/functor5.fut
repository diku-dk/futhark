-- Open and functors must work together.
-- ==
-- input {} output {6}

module type mt = { val x: i32 }

module m1: mt = { let x = 2 }

module f(M: mt) = {
  open M
  let y = x + 2
}

module m2 = f(m1)

let main = m2.x + m2.y
