-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

fun min(a: int) (b: int): int = if(a<b) then a else b
fun plus1(a:  []int,  b: []int): []int = [1]

struct M0 
  {
    fun min1(a: []int, b: []int): []int = zipWith min a b
    fun redmin1(a:  []int): int = reduce min 1200 a
    fun redmin2(a: [][]int): []int = map redmin1 a

    struct M1 
      {
        fun plus1(a:  []int,  b: []int): []int = zipWith (+) a b
        fun plus2(a: [][]int, b: [][]int): [][]int = map plus1 (zip a b)
      }

    fun replin(len: int) (a: []int): [][]int = replicate len a
  }

fun floydSbsFun(n: int, d: [][]int ): [][]int =
    let d3  = replicate n (transpose d)
    let d2  = map       (M0.replin(n)) d
    let abr = map M0.M1.plus2 (zip d3 d2)      
    let partial = map M0.redmin2 abr        in
        map M0.min1 (zip partial d )

fun main(): [][]int =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
