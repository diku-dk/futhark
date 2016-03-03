-- --
-- input {
-- }
-- output {
--   11
-- }
fun int max(int x, int y) = if(x<y) then y else x

fun {int,int,int, int} mapfun(int x) =
    let xm = max(x, 0) in
        {xm, xm, xm, x}

fun {int, int, int, int} redop({int,int,int,int} a, {int,int,int,int} b) =
    let {a0,a1,a2,a3} = a in
    let {b0,b1,b2,b3} = b in
    let mss = max( max(a0, b0), a2 + b1 ) in
    let mis = max( a1, a3 + b1 )          in
    let mcs = max( a2 + b3, b2 )          in
    let ts  = a3 + b3                     in
        {mss, mis, mcs, ts}


fun {int,int,int,int} mssp([int] inarr) =
    let maparr = map(mapfun, inarr) in
        reduce(redop, {0, 0, 0, 0}, maparr)

fun int main() =
    let inarr  = [ 1, -2, 3, 4, -1, 5, -6, 1 ] in
    let {res,t1,t2,t3} = mssp(inarr)           in
    res
