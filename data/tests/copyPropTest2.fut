-- ==
-- input {
-- }
-- output {
--   91
--   126
-- }
fun int getInt ( ) = 10
fun [int] plus1([int] x) = map(fn int(int y)=>y+1, x)

fun (int,int) main() =
    let n  = getInt()          in   -- Int
    let x  = iota(n)     in   -- [Int,n]
    let m  = (n * (5-4))       in
    let y  = copy(replicate(n,   x)) in   -- [[Int,n],n] copy necessary as y otherwise aliases x.
    let z  = copy(replicate(n+n, y)) in   -- [[[Int,n],n],m+n]; copy necessary as z otherwise aliases x.
    let q  = z[n-2]            in   -- [[Int,n],n]

    loop ((m,x)) =
        for i < n-1 do
            let x[i] = (m*1)            in
            let m    = m + x[i+1]       in
            let m    = m + z[n-1,n-2,i] in
                (m, x)
    in let qq = m*(2-1) in
        (qq, m + x[n/2])
