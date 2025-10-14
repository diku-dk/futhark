
type num = f32

-- assumes `x` has been already permuted (under bit-reverse)
-- Precondition: lgn >= 1
def fft2Par (lgn: { i64 | \ x -> x > 0 }) (omega: num) (x: *[1<<lgn]num) 
            : { *[1<<lgn]num | \ _ -> true } =
    let n = 1 << lgn  -- 2^lgn
    let omega_pows = scan (*) 1 (map (\i -> if i==0 then 1 else omega) (iota n)) 
    in
      loop (x : *[1<<lgn]num) -- this should be [n]num
      for qm1 < lgn do
        let q = qm1 + 1   -- q in [1..lgn]
        let L = 1i64 << q -- 2^q
        let Ld2 = L / 2   -- 2^{q-1}
        let r = n / L     -- 2^{lgn - q}
            
        let res_nested =
           map (\k -> 
             map (\j -> 
                    let kLj = k*L + j
                    let omega_pow = omega_pows[r*j]
                    let tau = omega_pow * x[kLj + Ld2]
                    let x_kLj = x[kLj]
                    in  (kLj, x_kLj + tau, kLj+Ld2, x_kLj - tau)
                 ) (iota Ld2)
               ) (iota r)
        --
        -- let (is1, vs1, is2, vs2) = unzip4 (flatten res_nested) 
        -- in  scatter x (is1++is2) (vs1++vs2)
        let (is1, vs1, is2, vs2) = unzip4 (map unzip4 res_nested) 
        in is1 -- is bijective in [0, n/2)
