-- Port of Ken Friis Larsens pricer for American Put Options:
--
-- https://github.com/kfl/american-options.
--
-- This implementation is a straightforward sequential port - it is
-- fairly slow on the GPU.
--
-- ==
-- tags { no_python }
-- compiled input { 1  } output { 6.745048f32 }
-- compiled input { 8  } output { 13.943413f32 }
-- compiled input { 16 } output { 16.218975f32 }
-- compiled input { 30 } output { 17.648781f32 }

-- constants

let strike(): i32 = 100
let bankDays(): i32 = 252
let s0(): i32 = 100
let r(): f32 = 0.03
let alpha(): f32 = 0.07
let sigma(): f32 = 0.20

let binom(expiry: i32): f32 =
  let n = expiry * bankDays()
  let dt = r32(expiry) / r32(n)
  let u = f32.exp(alpha()*dt+sigma()*f32.sqrt(dt))
  let d = f32.exp(alpha()*dt-sigma()*f32.sqrt(dt))
  let stepR = f32.exp(r()*dt)
  let q = (stepR-d)/(u-d)
  let qUR = q/stepR
  let qDR = (1.0-q)/stepR

  let np1 = n+1
  let uPow = map (u**) (map r32 (iota np1))
  let dPow = map (d**) (map r32 (map (n-) (iota np1)))
  let st = map (r32(s0())*) (map2 (*) uPow dPow)
  let finalPut = map (f32.max(0.0)) (map (r32(strike())-) st) in
  let put = loop put = finalPut for i in reverse (map (1+) (iota n)) do
    let uPow_start = take i uPow
    let dPow_end = drop (n+1-i) dPow :> [i]f32
    let st = map (r32(s0())*) (map2 (*) uPow_start dPow_end)
    let put_tail = tail put :> [i]f32
    let put_init = init put :> [i]f32 in
    map (\(x,y) -> f32.max x y)
    (zip
     (map (r32(strike())-) st)
     (map2 (+)
      (map (qUR*) (put_tail))
      (map (qDR*) (put_init))))
  in put[0]

let main(expiry: i32): f32 =
  binom(expiry)
