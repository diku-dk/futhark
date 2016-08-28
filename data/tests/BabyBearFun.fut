-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

--------------------------------------------------
-- SAC VERSIOn
--------------------------------------------------
--inline int[.,.] floydSbs1(int[.,.] d ) [
--    dT = transpose(d);
--    res = with
--        (. <= [i,j] <= .) :
--            min( d[i,j], minval( d[i] + dT[j]));
--        : modarray(d);
--    return( res);
--]



--------------------------------------------------
-- C VERSIOn
--------------------------------------------------
--inline int* floydSbs1( int n, int* d ) [
--    do k = 1, n
--      do i = 1, n
--        do j = 1, n
--          d[i,j] = min(d[i,j], d[i,k] + d[k,j])
--        enddo
--      enddo
--    enddo

--------------------------------------------------
-- C VERSIOn
--------------------------------------------------
--inline int* floydSbs1( int n, int* d ) [
--    do i = 1, n
--      do j = 1, n
--        minrow = 0;
--        do k = 1, n
--          minrow = min(minrow, d[i,k] + d[k,j])
--        enddo
--        d[i,j] = min(d[i,j], minrow)
--      enddo
--    enddo


fun min(a: int) (b: int): int = if(a<b) then a else b

fun min1(a: []int, b: []int): []int = map(fn (x,y) => min x y, zip(a, b))


fun redmin1(a:  []int): int = reduce(min, 1200, a)
fun redmin2(a: [][]int): []int = map   (redmin1, a)

fun plus1(a:  []int,  b: []int): []int = map((+), zip(a, b))
fun plus2(a: [][]int, b: [][]int): [][]int = map(plus1, zip(a, b))

fun replin(len: int) (a: []int): [][]int = replicate len a

fun floydSbsFun(n: int, d: [][]int ): [][]int =
    let d3  = replicate n (transpose d) in
    let d2  = map      ( replin(n),   d  ) in
    let abr = map(plus2, zip(d3, d2))       in
    let partial = map(redmin2, abr)        in
        map(min1, zip(partial, d) )

fun main(): [][]int =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
