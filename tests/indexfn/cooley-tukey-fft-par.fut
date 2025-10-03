def loopbody (lgn: { i64 | \x -> x > 0 }) (x: [1<<lgn]f32)
             (qm1: { i64 | \x -> Range x (0,lgn) })
             (omega_pows: []f32) : ({[][]i64 | \is1 -> true}, [][]f32, [][]i64, [][]f32) =
  let q = qm1 + 1   -- q in [1..lgn]
  let L = 1i64 << q -- 2^q
  let Ld2 = L / 2   -- 2^{q-1}
  let r = (1 << lgn) / L     -- 2^{lgn - q}
      
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
  let (is1, vs1, is2, vs2) = unzip4 (map (\x -> unzip4 x) res_nested)
  in  (is1, vs1, is2, vs2)
  -- in is1 -- Property to prove: is bijective in [0, n/2)

-- assumes `x` has been already permuted (under bit-reverse)
-- Precondition: lgn >= 1
def fft2Par (lgn: { i64 | \ x -> x > 0 }) (omega: f32) (x: *[1<<lgn]f32) 
            : { *[1<<lgn]f32 | \ _ -> true } =
    let n = 1 << lgn  -- 2^lgn
    let omega_pows = scan (*) 1 (map (\i -> if i==0 then 1 else omega) (iota n))
    in
      loop (x : *[1<<lgn]f32) -- this should be [n]f32
      for qm1 < lgn do
        let (is1, vs1, is2, vs2) = loopbody lgn x qm1 omega_pows
        in  scatter x (flatten is1++flatten is2) (flatten vs1++flatten vs2)
        -- let q = qm1 + 1   -- q in [1..lgn]
        -- let L = 1i64 << q -- 2^q
        -- let Ld2 = L / 2   -- 2^{q-1}
        -- let r = n / L     -- 2^{lgn - q}
            
        -- let res_nested =
        --    map (\k -> 
        --      map (\j -> 
        --             let kLj = k*L + j
        --             let omega_pow = omega_pows[r*j]
        --             let tau = omega_pow * x[kLj + Ld2]
        --             let x_kLj = x[kLj]
        --             in  (kLj, x_kLj + tau, kLj+Ld2, x_kLj - tau)
        --          ) (iota Ld2)
        --        ) (iota r)
        -- --
        -- let (is1, vs1, is2, vs2) = unzip4 (flatten res_nested)
        -- in  scatter x (is1++is2) (vs1++vs2)
