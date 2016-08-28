-- ==
-- input {
-- }
-- output {
--   91
--   126
-- }
-- structure { Replicate 0 }
fun getInt (): int = 10
fun plus1(x: []int): []int = map(fn (y: int): int=>y+1, x)

fun main(): (int,int) =
    let n  = getInt()          in   -- Int
    let x  = iota(n)     in   -- [n]Int
    let m  = (n * (5-4))       in
    let y  = copy(replicate n x) in   -- [n][n]Int copy necessary as y otherwise aliases x.
    let z  = copy(replicate (n+n) y) in   -- [[n][n]Int,m+n]; copy necessary as z otherwise aliases x.
    let q  = z[n-2]            in   -- [n][n]Int

    loop ((m,x)) =
        for i < n-1 do
            let x[i] = (m*1)            in
            let m    = m + x[i+1]       in
            let m    = m + z[n-1,n-2,i] in
                (m, x)
    in let qq = m*(2-1) in
        (qq, m + x[n/2])
