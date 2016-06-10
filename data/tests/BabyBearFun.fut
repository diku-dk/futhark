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


fun int min(int a, int b) = if(a<b) then a else b

fun [int] min1([int] a, [int] b) = map(min, zip(a, b))


fun   int    redmin1( [int]  a) = reduce(min, 1200, a)
fun  [int]   redmin2([[int]] a) = map   (redmin1, a)

fun  [int]   plus1( [int]  a,  [int]  b) = map(+, zip(a, b))
fun [[int]]  plus2([[int]] a, [[int]] b) = map(plus1, zip(a, b))

fun [[int]]  replin(int len, [int] a) = replicate(len, a)

fun [[int]] floydSbsFun(int n, [[int]] d ) =
    let d3  = replicate( n, transpose(d) ) in
    let d2  = map      ( replin(n),   d  ) in
    let abr = map(plus2, zip(d3, d2))       in
    let partial = map(redmin2, abr)        in
        map(min1, zip(partial, d) )

fun [[int]] main() =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
