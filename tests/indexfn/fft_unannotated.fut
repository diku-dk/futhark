def loop_body
    (n: i64)
    (omega_pows: [1<<n]f32)
    (x: *[1<<n]f32)
    (q: i64)
    :  *[1<<n]f32 =
  let L = 1i64 << (q + 1)
  let r = (1 << n) / L
  let res_nested =
     map (\k ->
       map (\j ->
              let tau = omega_pows[r*j] * x[k*L+j+L/2]
              in  (k*L+j, x[k*L+j] + tau, k*L+j+L/2, x[k*L+j] - tau)
           ) (iota (L/2))
         ) (iota r)
  let (is1, vs1, is2, vs2) = unzip4 (flatten res_nested)
  let is = is1 ++ is2
  let vs = vs1 ++ vs2
  let x' = scatter x is vs
  in x'


-- Assumes `x` has been already permuted (under bit-reverse) and that omega_pows
-- = scan (*) 1 (map (\i -> if i==0 then 1 else omega) (iota (1<<n))) has
-- already been computed.
-- Precondition: n >= 1
def fft (n:  i64) (omega_pows: [1<<n]f32) (x: *[1<<n]f32)
            :  *[1<<n]f32 =
    loop (x : *[1<<n]f32) for q < n do
      loop_body n omega_pows x q
