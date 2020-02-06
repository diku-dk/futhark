-- Test a tricky case involving rewriting lambda arguments in the
-- tuple transformer.
-- ==
-- input {
--   [[1,5],[8,9],[2,4]]
--   [[5,1],[9,2],[4,8]]
-- }
-- output {
--   [6, 17, 6]
-- }

let inner(a: [][](i32,i32)): []i32 =
  map (\(r: [](i32,i32)): i32 -> let (x,y) = r[0] in x+y) a

let main (a1: [][]i32) (a2: [][]i32): []i32 =
  let a = map (\(p: ([]i32,[]i32)) ->
                let (p1,p2) = p in
                zip p1 p2) (
              zip a1 a2) in
  inner(a)
