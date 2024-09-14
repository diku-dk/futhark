import "../../accs/intrinsics"

def update (T:i64) (R:i64) (Oacc: *acc ([T*R]f32)) (tid: i64, a: f32, Qk: [R]f32) : *acc ([T*R]f32) = #[unsafe]
  loop Oacc for i < R do
    let elm = Qk[i] * a
    let ind = i*T + tid
    in  write Oacc ind elm

entry main (T: i64) (R: i64) (Q: [T][R]f32) (A: [T]f32) (O: *[T*R]f32) : *[T*R]f32 = #[unsafe]
  let A' = map (*2.0) A in
  let z3 = zip3 (iota T) A' Q
  in  scatter_stream O (update T R) z3


