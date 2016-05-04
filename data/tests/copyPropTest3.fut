-- ==
-- input {
-- }
-- output {
--   70
-- }
fun int getInt() = 10

fun int myfun( (int,int,(int,int)) x ) =
    let (a,b,(c,d)) = x in a + b + c + d

fun int main() =
    let n  = getInt()          in   -- Int
    let a  = (n, n, (n*0+5,n))   in

    let (x1, x2) = (
                replicate(n, a),
                [ a, a, a, a, a, a, a, a, a, a ]
            )                  in
    let y1  = replicate(n, x1) in
    let y2  = replicate(n, x2) in
    let z   = y1[n-1]          in
    let (b,c,(d,e))  = z[n/2]  in
    let m  = y2[0, (5-1)/2]    in
    let p  = m                 in
      b + c + d + e + myfun(p)
