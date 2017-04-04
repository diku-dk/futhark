-- Tests some nasty code generation/simplification details about
-- removing existential contexts.
--
-- ==
--
-- input { true 3 }
-- output { [0,1,2] }
-- input { false 3 }
-- output { [1337,1337,1337] }

let f(a: [n]i32): []i32 = a

let g(n: i32): []i32 = replicate n 1337

let main(b: bool, n: i32): []i32 =
  let a = iota(n) in
  if b then f(a) else g(n)
