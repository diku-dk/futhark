-- TODO extract loop body into a function; otherwise I dont think
-- the analysis is sane? do the index functions inside reflect that
-- there may be several iterations? i.e., x cannot be assumed
-- to have its initial value for the analysis, but should
-- instead be treated as an argument.
-- This should really be done in Convert.hs so as to not
-- make such a mistake.
--

-- def cat [n][m] 't (xs: [n]t) (ys: [m]t): [n+m]t =
--   map (\i -> if i < n then xs[i] else ys[i - n]) (iota (n+m))

def loop_body (lgn: i64) (x: *[1<<lgn]f32) (qm1: {i64 | \qm1' -> Range qm1' (0,lgn)}) (omega_pows: [1<<lgn]f32): {*[1<<lgn]f32 | \_ -> true} =
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

    let (is1, vs1, is2, vs2) = unzip4 (flatten res_nested)
    let is = is1 ++ is2
    let vs = vs1 ++ vs2
    -- let is = cat is1 is2
    -- let vs = cat vs1 vs2
    let x' = scatter x is vs
    in  x'

-- assumes `x` has been already permuted (under bit-reverse)
-- Precondition: lgn >= 1
def fft2Par (lgn: { i64 | \ x -> x > 0 }) (omega: f32) (x: *[1<<lgn]f32)
            : { *[1<<lgn]f32 | \ _ -> true } =
    let n = 1 << lgn  -- 2^lgn
    let omega_pows = scan (*) 1 (map (\i -> if i==0 then 1 else omega) (iota n)) 
    in
      loop (x : *[1<<lgn]f32) -- this should be [n]f32
      for qm1 < lgn do
        loop_body lgn x qm1 (sized (1 << lgn) omega_pows)
