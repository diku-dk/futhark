-- Test scanomap fusion and scan kernel generation with map-outs.
--
-- ==
-- input { 10 100 } output { 12 55 }
-- structure { Scan 0 Map 0 Scanomap 1 }

let main(i: i32, n: i32): (i32, i32) =
  let a = iota(n)
  let b = map (+2) a
  let c = scan (+) 0 a
  in (b[i], c[i])
