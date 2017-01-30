-- Port of Ken Friis Larsens pricer for American Put Options:
--
-- https://github.com/kfl/american-options.
--
-- This implementation is a straightforward sequential port - it is
-- fairly slow on the GPU.
--
-- ==
-- tags { no_python }
--          input { 1  } output { 6.745433 }
-- compiled input { 8  } output { 13.945689 }
-- compiled input { 16 } output { 16.222591 }
-- compiled input { 30 } output { 17.653706 }
-- compiled input { 64 } output { 18.429932 }

-- constants

import "futlib/numeric"

fun strike(): i32 = 100
fun bankDays(): i32 = 252
fun s0(): i32 = 100
fun r(): f64 = f64(0.03)
fun alpha(): f64 = f64(0.07)
fun sigma(): f64 = f64(0.20)

fun maxf64(x: f64) (y: f64): f64 =
  if x < y then y else x

fun binom(expiry: i32): f64 =
  let n = expiry * bankDays()
  let dt = f64(expiry) / f64(n)
  let u = f64.exp(alpha()*dt+sigma()*f64.sqrt(dt))
  let d = f64.exp(alpha()*dt-sigma()*f64.sqrt(dt))
  let stepR = f64.exp(r()*dt)
  let q = (stepR-d)/(u-d)
  let qUR = q/stepR
  let qDR = (f64(1.0)-q)/stepR

  let uPow = map (u**) (map f64 (iota(n+1)))
  let dPow = map (d**) (map f64 (map (n-) (iota(n+1))))
  let st = map (f64(s0())*) (map (*) uPow dPow)
  let finalPut = map (maxf64(f64(0.0))) (map (f64(strike())-) st) in
  loop (put = finalPut) = for (n+1) > i >= 1 do
    let (uPow_start, _) = split (i) uPow
    let (_, dPow_end) = split (n+1-i) dPow
    let st = map (f64(s0())*) (map (*) uPow_start dPow_end)
    let (_, put_tail) = split (1) put
    let (put_init, _) = split ((shape put)[0]-1) put in
    map (\(x,y) -> maxf64 x y)
    (zip
     (map (f64(strike())-) st)
     (map (+)
      (map (qUR*) (put_tail))
      (map (qDR*) (put_init)))) in
  put[0]

fun main(expiry: i32): f64 =
  binom(expiry)
