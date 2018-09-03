-- Basic/naive use of ascription.
-- ==
-- input {} output { 2 }

module m = { let x = 2 }
module m': { val x: i32 } = m

let main = m'.x
