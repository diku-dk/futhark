-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

fun min(a: i32) (b: i32): i32 = if(a<b) then a else b
fun plus1(a:  []i32,  b: []i32): []i32 = [1]

module M0
  {
    fun min1(a: []i32, b: []i32): []i32 = map min a b
    fun redmin1(a:  []i32): i32 = reduce min 1200 a
    fun redmin2(a: [][]i32): []i32 = map redmin1 a

    module M1
      {
        fun plus1(a:  []i32,  b: []i32): []i32 = map (+) a b
        fun plus2(a: [][]i32, b: [][]i32): [][]i32 = map plus1 (zip a b)
      }

    fun replin(len: i32) (a: []i32): [][]i32 = replicate len a
  }

fun floydSbsFun(n: i32, d: [][]i32 ): [][]i32 =
    let d3  = replicate n (transpose d)
    let d2  = map       (M0.replin(n)) d
    let abr = map M0.M1.plus2 (zip d3 d2)
    let partial = map M0.redmin2 abr        in
        map M0.min1 (zip partial d )

fun main(): [][]i32 =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
