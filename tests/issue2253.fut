type v3 = (f32, f32, f32)
type v3s [n] = [n]v3
type v3pairs [n] = [n](v3, v3)

entry fromV3pairs (vps: v3pairs []) : (v3s [], v3s []) = unzip vps

type Anderson_state [m] [n] =
  ( [m]v3pairs [n]
  , i64
  )

entry initialize_Anderson m n : Anderson_state [m] [n] =
  (replicate m (replicate n ((0, 0, 0), (0, 0, 0))), 0)
