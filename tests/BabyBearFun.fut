-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

--------------------------------------------------
-- SAC VERSIOn
--------------------------------------------------
--inline i32[.,.] floydSbs1(i32[.,.] d ) [
--    dT = transpose(d);
--    res = with
--        (. <= [#i,j] <= .) :
--            min( d[i,j], minval( d[i] + dT[j]));
--        : modarray(d);
--    return( res);
--]



--------------------------------------------------
-- C VERSIOn
--------------------------------------------------
--inline i32* floydSbs1( i32 n, i32* d ) [
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
--inline i32* floydSbs1( i32 n, i32* d ) [
--    do i = 1, n
--      do j = 1, n
--        minrow = 0;
--        do k = 1, n
--          minrow = min(minrow, d[i,k] + d[k,j])
--        enddo
--        d[i,j] = min(d[i,j], minrow)
--      enddo
--    enddo


let min1 [n] (a: [n]i32, b: [n]i32): [n]i32 = map (uncurry i32.min) (zip a b)

let redmin1(a:  []i32): i32 = reduce i32.min 1200 a
let redmin2 [n][m] (a: [n][m]i32): [n]i32 = map redmin1 a

let plus1 [n] (a:  [n]i32,  b: [n]i32): [n]i32 = map2 (+) a b
let plus2 [n][m] (a: [n][m]i32, b: [n][m]i32): [n][m]i32 = map plus1 (zip a b)

let replin [k] (len: i32) (a: [k]i32): [len][k]i32 = replicate len a

let floydSbsFun (n: i32) (d: [n][n]i32 ): [][]i32 =
    let d3  = replicate n <| transpose d
    let d2  = map        (replin(n)) d
    let abr = map plus2 (zip d3 d2)
    let partial = map redmin2 abr        in
        map min1 (zip partial d )

let main: [][]i32 =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun 3 arr
