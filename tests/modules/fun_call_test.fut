-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

let min(a: i32) (b: i32): i32 = if(a<b) then a else b
let plus1(a:  []i32,  b: []i32): []i32 = [1]

module M0 = {
    let min1 [n] (a: [n]i32, b: [n]i32) = map2 min a b
    let redmin1(a:  []i32): i32 = reduce min 1200 a
    let redmin2 [n] (a: [n][]i32): [n]i32 = map redmin1 a

    module M1 = {
        let plus1 [n] (a:  [n]i32,  b: [n]i32): [n]i32 = map2 (+) a b
        let plus2 [n][k] (a: [n][k]i32, b: [n][k]i32): [n][k]i32 = map plus1 (zip a b)
      }

    let replin [k] (len: i32) (a: [k]i32): [len][k]i32 = replicate len a
  }

let floydSbsFun (n: i32) (d: [n][n]i32 ): [][]i32 =
    let d3  = replicate n (transpose d)
    let d2  = map       (M0.replin n) d
    let abr = map M0.M1.plus2 (zip d3 d2)
    let partial = map M0.redmin2 abr
    in map M0.min1 (zip partial d )

let main: [][]i32 =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun 3 arr
