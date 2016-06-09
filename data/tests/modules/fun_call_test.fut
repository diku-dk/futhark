-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

fun int MIN(int a, int b) = if(a<b) then a else b
fun [int] plus1( [int]  a,  [int]  b) = [1]

struct M0 
  {
    fun [int] min1([int] a, [int] b) = map(MIN, zip(a, b))
    fun  int  redmin1( [int]  a) = reduce(MIN, 1200, a)
    fun [int] redmin2([[int]] a) = map(redmin1, a)

    struct M1 
      {
        fun  [int]   plus1( [int]  a,  [int]  b) = map(+, zip(a, b))
        fun [[int]]  plus2([[int]] a, [[int]] b) = map(plus1, zip(a, b))
      }

    fun [[int]] replin(int len, [int] a) = replicate(len, a)
  }

fun [[int]] floydSbsFun(int N, [[int]] D ) =
    let D3  = replicate( N, transpose(D) ) in
    let D2  = map      ( M0.replin(N),   D  ) in
    let abr = map(M0.M1.plus2, zip(D3, D2))       in
    let partial = map(M0.redmin2, abr)        in
        map(M0.min1, zip(partial, D) )

fun [[int]] main() =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
