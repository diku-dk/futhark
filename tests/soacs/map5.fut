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
  map (\(r1: [](i32,i32)): i32 ->
        let r2 = r1
        let (x,y) = r2[0] in
        x+y) a

let main(a1: [][]i32) (a2: [][]i32): []i32 =
  inner(map (\(r: ([]i32,[]i32)) ->
              let (r1,r2) = r in
              zip r1 r2) (
            zip a1 a2))
