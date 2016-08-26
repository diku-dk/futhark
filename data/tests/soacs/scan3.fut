-- Test scanomap fusion and scan kernel generation with map-outs.
--
-- ==
-- input { 10 100 } output { 12 55 }
-- structure { Scan 0 Map 0 Scanomap 1 }

fun main(i: int, n: int): (int, int) =
  let a = iota(n)
  let b = map(+2, a)
  let c = scan(+, 0, a)
  in (b[i], c[i])
