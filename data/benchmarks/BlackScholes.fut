
fun real horner (real x) =
   let {c1,c2,c3,c4,c5} = {0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429}
   in x * (c1 + x * (c2 + x * (c3 + x * (c4 + x * c5))))

fun real abs (real x) = if x < 0.0 then -x else x

fun real cnd0 (real d) =
   let k        = 1.0 / (1.0 + 0.2316419 * abs(d)) in   
   let p        = horner(k) in
   let rsqrt2pi = 0.39894228040143267793994605993438 in
   rsqrt2pi * exp(-0.5*d*d) * p

fun real cnd (real d) =
   let c = cnd0(d)
   in if 0.0 < d then 1.0 - c else c 

fun real go ({bool,real,real,real} x) =
   let {call, price, strike, years} = x in
   let r       = 0.08 in  // riskfree
   let v       = 0.30 in  // volatility
   let v_sqrtT = v * sqrt(years) in
   let d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT in
   let d2      = d1 - v_sqrtT in
   let cndD1   = cnd(d1) in
   let cndD2   = cnd(d2) in
   let x_expRT = strike * exp (-r * years) in
   if call then
     price * cndD1 - x_expRT * cndD2
   else
     x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1)

fun [real] blackscholes ([{bool,real,real,real}] xs) = 
   map (go, xs)

fun [real] main () =
  let days = 5*365 in
  let a = map(op+(1), iota(days)) in
  let a = map(toReal, a) in
  let a = map(fn {bool,real,real,real} (real x) => {True, 58.0 + 4.0 * x / toReal(days), 65.0, x / 365.0}, a) in
  blackscholes(a) 