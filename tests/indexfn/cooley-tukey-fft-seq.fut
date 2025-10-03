
type num = f32

def pow (x: num) (n: i64) : num =
  loop r = 1f32 for _i < n do r * x

-- assumes `x` has been already permuted (under bit-reverse)
def fftSeq (lgn: i64) (omega: num) (x: *[1<<lgn]num) : *[1<<lgn]num =
  let n = 1 << lgn
  in
  loop (x) for qm1 < lgn do
    let q = qm1 + 1
    let L = 1i64 << q
    let r = n / L
    in
    loop (x) for k < r do
      let omega_pow = 1
      let omega_step = pow omega r
      let (x, _) = 
        loop (x, omega_pow) for j < L/2 do
          let omega_pow = 
                if j == 0 then omega_pow
                else omega_pow * omega_step
          let kLj = k*L + j
          let tau = omega_pow * x[kLj + L/2]
          let x[kLj]       = x[kLj] + tau
          let x[kLj + L/2] = x[kLj] - tau
          in  (x, omega_pow)
      in  x
