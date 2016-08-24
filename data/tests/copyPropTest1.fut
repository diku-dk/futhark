-- ==
-- input {
-- }
-- output {
--   52
-- }
-- structure { Replicate 0 }
fun int getInt ( ) = if((1-1)*3 + (3/3 - 1) == 0) then (15 / 3)*2 else 10000000
fun []int plus1([]int x) = map(fn int(int y)=>y+1, x)

fun int main() =
    let n  = getInt()          in   -- Int
    let x  = iota(n)           in   -- [n]Int
    let m  = (n*1)+(n*0)       in   -- n :: Int
    let y  = replicate(m,   x) in   -- [n][n]Int
    let u  = map(plus1, y)     in   -- [n][n]Int
    let z  = replicate(m+n, y) in   -- [[n][n]Int,m+n]
    let v  = u[m/2-1]          in   -- [n]Int
    let o  = (m +(2-4/2))*1    in   -- n :: Int
    let q  = z[m/2]            in   -- [n][n]Int
    let t  = q[m/3, m/4]       in   -- n/4 :: Int
    let w  = x[m-n/2]          in   -- n-n/2 :: Int
    let s  = v[3]              in   -- u[m+n/2,3] :: Int
        x[m*n/n - 1] + y[o-1,if(o*(3-3) == 0) then o-1 else m*n*o] + u[0, m-n] +
        z[(1+3)/m, if(False || o*(3+-9/3)==0) then 3/5 else (4+2)/3, (m*1)/2 ] +
        s + t + q[0,n*1-1] + o + v[2] + (m - n + 0)
