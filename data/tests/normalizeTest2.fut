-- ==
-- input {
--   42
-- }
-- output {
--   72754416
-- }
fun int main(int a) =
    let y = let zz = 2*a in copy(replicate(zz,a)) in
    let bound = let x1 = 2*y[2*a-a-a] in 2*x1 in
    loop (res=a) = for i < bound do
        let y = let zz = 2*a in copy(replicate(zz,a)) in
        let z = (let y[0] =
                    (let x = 5*a-7 in a*x - 3*a*y[let q = x/3 in q-a])
                    in a* reduce( +, a+reduce( +, 0, map( +1, y) ), let qq = 2*a in map(fn int (int q) => qq+q, y) )
                )
        in z*a
    in res*2
