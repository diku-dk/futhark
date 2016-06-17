-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

fun int min(int a, int b) = if(a<b) then a else b
fun []int plus1( []int  a,  []int  b) = [1]

struct M0 
  {
    fun []int min1([]int a, []int b) = map(min, zip(a, b))
    fun  int  redmin1( []int  a) = reduce(min, 1200, a)
    fun []int redmin2([][]int a) = map(redmin1, a)

    struct M1 
      {
        fun  []int   plus1( []int  a,  []int  b) = map(+, zip(a, b))
        fun [][]int  plus2([][]int a, [][]int b) = map(plus1, zip(a, b))
      }

    fun [][]int replin(int len, []int a) = replicate(len, a)
  }

fun [][]int floydSbsFun(int n, [][]int d ) =
    let d3  = replicate( n, transpose(d) ) in
    let d2  = map      ( M0.replin(n),   d  ) in
    let abr = map(M0.M1.plus2, zip(d3, d2))       in
    let partial = map(M0.redmin2, abr)        in
        map(M0.min1, zip(partial, d) )

fun [][]int main() =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
