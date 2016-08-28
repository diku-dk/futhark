-- ==
-- input { [1.0, 2.0] [[1.0, 2.0], [-4.0, 1.5]] }
-- output { 8.0 -1.0 0.0 -1.0 5.5 }
-- structure { Redomap 1 Reduce 0 Map 0 }

fun f1(p:  (f64,f64) ): (f64,f64,f64) =
    let (a1, a2) = p in (a1 * a2, a1 + a2, a1 - a2)

fun f2(p:  (f64,f64) ): (f64,f64) =
    let (a1, a2) = p in (a1 - a2, a1 + 2.0*a2)

fun g (p:  (f64,f64,f64,f64) ): (f64,f64) =
    let (a1,a2,a3,a4) = p in (a1 * a2 - a3 * a4, a3 + a4 + a2 - a1)

--fun f64 myop ( (f64,f64,f64,f64,f64) p ) =
--    let {a1,a2,a3,a4,a5} = p in a1+a2+a3+a4+a5


fun myop (p: (f64,f64,f64,f64,f64)) (q: (f64,f64,f64,f64,f64)): (f64,f64,f64,f64,f64) =
    let (a1,a2,a3,a4,a5) = p in
    let (b1,b2,b3,b4,b5) = q in
        (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5)

--fun f64
fun main(x1: []f64, x2: [][]f64): (f64,f64,f64,f64,f64) =
    let (y1, y2, y3) = unzip( map f1 (zip(x1, x2[1]  ) ) )  in
    let (z1, z2)     = unzip( map f2 (zip(y1, y2     ) ) )  in
    let (q1, q2)     = unzip( map g (zip(y3,z1,y2,y3) ) )  in
--    let res          = map  ( myop, zip(q1,q2,z2,y1,y3) )    in
--        res[3]
    reduce myop (0.0,0.0,0.0,0.0,0.0) (zip(q1, q2, z2, y1, y3) )
