-- One size-parameterised type refers to another.
-- ==
-- input { empty([0]i32) empty([0]i32) } output { empty([0]i32) empty([0]i32) }
-- input { [1,2,3] [1,2,3] } output { [1,2,3] [1,2,3] }
-- input { [1,2,3] [1,2,3,4] } output { [1,2,3] [1,2,3] }

type ints [n] = [n]i32

type pairints [n] [m] = (ints [n], ints [m])

let main [n][m] (a: ints [n]) (b: ints [m]) : pairints [n] [n] =
  let b' = (split n b).1
  in (a,b')
