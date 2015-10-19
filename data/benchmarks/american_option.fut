-- Port of Ken Friis Larsens pricer for American Put Options:
--
-- https://github.com/kfl/american-options.
--
-- This implementation is a straightforward sequential port - it is
-- fairly slow on the GPU.
--
-- ==
--
-- compiled input { 1  } output { 6.745433 }
-- compiled input { 8  } output { 13.945689 }
-- compiled input { 16 } output { 16.222591 }
-- compiled input { 30 } output { 17.653706 }
-- compiled input { 64 } output { 18.429932 }

-- constants

fun int strike() = 100
fun int bankDays() = 252
fun int s0() = 100
fun real r() = 0.03
fun real alpha() = 0.07
fun real sigma() = 0.20

fun real maxReal(real x, real y) =
  if x < y then y else x

fun real binom(int expiry) =
  let n = expiry * bankDays() in
  let dt = toFloat(expiry) / toFloat(n) in
  let u = exp(alpha()*dt+sigma()*sqrt(dt)) in
  let d = exp(alpha()*dt-sigma()*sqrt(dt)) in
  let stepR = exp(r()*dt) in
  let q = (stepR-d)/(u-d) in
  let qUR = q/stepR in
  let qDR = (1.0-q)/stepR in

  let uPow = map(u pow, map(toFloat, iota(n+1))) in
  let dPow = map(d pow, map(toFloat, map(n-, iota(n+1)))) in
  let st = map(toFloat(s0())*, map(*, zip(uPow, dPow))) in
  let finalPut = map(maxReal(0.0), map(toFloat(strike())-, st)) in
  loop (put = finalPut) = for j < n do
    let i = n - j in
    let {uPow_start, _} = split((i), uPow) in
    let {_, dPow_end} = split((n+1-i), dPow) in
    let st = map(toFloat(s0())*, map(*, zip(uPow_start, dPow_end))) in
    let {_, put_tail} = split((1), put) in
    let {put_init, _} = split((size(0,put)-1), put) in
    map(maxReal, zip(map(toFloat(strike())-, st),
                     map(+,
                         zip(map(qUR*, put_tail),
                             map(qDR*, put_init))))) in
  put[0]

fun real main(int expiry) =
  binom(expiry)