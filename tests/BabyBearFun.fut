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


let min(a: i32) (b: i32): i32 = if(a<b) then a else b

let min1(a: []i32, b: []i32): []i32 = map (\(x,y) -> min x y) (zip a b)


let redmin1(a:  []i32): i32 = reduce min 1200 a
let redmin2(a: [][]i32): []i32 = map    redmin1 a

let plus1(a:  []i32,  b: []i32): []i32 = map (+) a b
let plus2(a: [][]i32, b: [][]i32): [][]i32 = map plus1 (zip a b)

let replin(len: i32) (a: []i32): [][]i32 = replicate len a

let floydSbsFun(n: i32, d: [][]i32 ): [][]i32 =
    let d3  = replicate n (transpose d)
    let d2  = map       (replin(n)) d
    let abr = map plus2 (zip d3 d2)
    let partial = map redmin2 abr        in
        map min1 (zip partial d )

let main(): [][]i32 =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsFun(3, arr)
