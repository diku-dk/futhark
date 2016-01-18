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

fun int strike() = 100
fun int bankDays() = 252
fun int s0() = 100
fun f64 r() = 0.03
fun f64 alpha() = 0.07
fun f64 sigma() = 0.20

fun f64 maxF64(f64 x, f64 y) =
  if x < y then y else x

fun f64 binom(int expiry) =
  let n = expiry * bankDays() in
  let dt = f64(expiry) / f64(n) in
  let u = exp(alpha()*dt+sigma()*sqrt(dt)) in
  let d = exp(alpha()*dt-sigma()*sqrt(dt)) in
  let stepR = exp(r()*dt) in
  let q = (stepR-d)/(u-d) in
  let qUR = q/stepR in
  let qDR = (1.0-q)/stepR in

  let uPow = map(u **, map(f64, iota(n+1))) in
  let dPow = map(d **, map(f64, map(n-, iota(n+1)))) in
  let st = map(f64(s0())*, map(*, zip(uPow, dPow))) in
  let finalPut = map(maxF64(0.0), map(f64(strike())-, st)) in
  loop (put = finalPut) = for n+1 > i >= 1 do
    let {uPow_start, _} = split((i), uPow) in
    let {_, dPow_end} = split((n+1-i), dPow) in
    let st = map(f64(s0())*, map(*, zip(uPow_start, dPow_end))) in
    let {_, put_tail} = split((1), put) in
    let {put_init, _} = split((size(0,put)-1), put) in
    map(maxF64, zip(map(f64(strike())-, st),
                     map(+,
                         zip(map(qUR*, put_tail),
                             map(qDR*, put_init))))) in
  put[0]

fun f64 main(int expiry) =
  binom(expiry)
