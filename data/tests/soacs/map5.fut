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

fun inner(a: [][](int,int)): []int =
  map(fn (r1: [](int,int)): int =>
        let r2 = r1 in
        let (x,y) = r2[0] in
        x+y,
      a)

fun main(a1: [][]int, a2: [][]int): []int =
  inner(map(fn (r: ([]int,[]int)): [](int,int)  =>
              let (r1,r2) = r in
              zip(r1,r2),
            zip(a1,a2)))
