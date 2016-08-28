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

fun floydSbsImp(n: int, d: *[][]int): [][]int =
    let dT = copy(transpose(d)) in
    loop (d) = for i < n do
        loop (d) = for j < n do
            let sumrow = map (+) (zip(d[i], dT[j])) in
            let minrow = reduce min 1200 sumrow    in
            let minrow = min d[i,j] minrow        in
            let d[i,j] = minrow in d
        in d
    in d

fun main(): [][]int =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsImp(3, copy(arr))
