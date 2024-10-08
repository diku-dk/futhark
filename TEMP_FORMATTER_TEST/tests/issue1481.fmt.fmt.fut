module type field =
{
  module R: real
  type t
  val zero: t
  val (+): t -> t -> t
  val (*): R.t -> t -> t
  -- dummy function to generate new non-zero values of for t
  val tab3: i64 -> i64 -> i64 -> t
}

module mk_scalar_field (R: real) = {
  module R = R
  
  type t = R.t
  
  def zero = R.i64 0
  
  def (+) (x: t) (y: t): t = R(x + y)
  
  def (*) (a: R.t) (x: t): t = R(a * x)
  
  def tab3 i j k: t = R((i64 i) + (i64 j) + (i64 k))
}

module mk_lt (F: field) = {
  module R = F.R
  
  def rm1 = R.i64 (-1)
  
  def r0 = R.i64 0
  
  def r1 = R.i64 1
  
  def r2 = R.i64 2
  
  def r3 = R.i64 3
  
  def r4 = R.i64 4
  
  def len_q (N: i64): i64 = (N + 1) * (N + 2) // 2
  
  def gen_ml (N: i64): [](i64, i64) =
    loop ml = [(0, 0)] for i < ((len_q N) - 1) do
      let (m, l) = ml[i]
      let nl = if N == l then m + 1 else l + 1
      let nm = if N == l then m + 1 else m
      in ml ++ [(nm, nl)]
  
  def all_amm (N: i64): []R.t =
    iota N
    |> map (\i -> R.i64 (i + 1))
    |> map (\k -> R((r2 * k + r1) / (r2 * k)))
    |> ([r1] ++)
    |> scan R.(*) r1
    |> map (\el -> R(sqrt (el / (r4 * pi))))
  
  def amn (m: R.t) (n: R.t): R.t = R(sqrt ((r4 * n * n - r1) / (n * n - m * m)))
  
  def bmn (m: R.t) (n: R.t): R.t =
    let l = R((r2 * n + r1) / (r2 * n - r3))
    let r = R(((n - r1) * (n - r1) - m * m) / (n * n - m * m))
    in R(rm1 * sqrt (l * r))
  
  def lat_grid (nlat: i64): []R.t =
    iota nlat
    |> map R.i64
    |> map (\x -> R(cos (x / (i64 nlat) * pi)))
  
  -- m<n
  def Lmx' [nlat] (m: i64) (np1: i64) (amm: R.t) (cx: [nlat]R.t) (x: [nlat]F.t): [np1]F.t =
    let n = np1 - 1
    let X = tabulate np1 (\i -> F.zero)
    let m' = R.i64 m
    let Sx p= map2 F.(*) p x |> reduce F.(+) F.zero
    -- P^m_m
    let p0 = map (\cx -> R(amm * (r1 - cx * cx) ** (m' / r2) * (rm1) ** m')) cx
    let X[m] = Sx p0
    -- P^m_(m + 1)
    let p1 = map2 (\cx p0 -> R((amn m' (m' + r1)) * cx * p0)) cx p0
    let X[m + 1] = Sx p1
    -- P^m_n -> P^m_n+1 -> P^m_n+2
    let p2 n p1 p0= map3 (\cx p1 p0 -> R((amn m' n) * cx * p1 + (bmn m' n) * p0)) cx p1 p0
    -- P^m_n
    let (X, pn, _) =
      match (n - m)
        case 0 -> (X, p0, p0)
        case 1 -> (X, p1, p0)
        case _ -> loop (X, p1, p0) = (X, p1, p0) for i < (n - m - 1) do
            let pi = p2 R(m' + r2 + i64 i) p1 p0
            let X[m + 2 + i] = Sx pi
            in (X, pi, p1)
    in X
  
  -- n==m and m<n
  def Lmx [nlat] (m: i64) (np1: i64) (amm: R.t) (cx: [nlat]R.t) (x: [nlat]F.t): [np1]F.t =
    let n = np1 - 1
    let X = tabulate np1 (\i -> F.zero)
    let m' = R.i64 m
    let Sx p= map2 F.(*) p x |> reduce F.(+) F.zero
    -- P^m_m
    let p0 = map (\cx -> R(amm * (r1 - cx * cx) ** (m' / r2) * (rm1) ** m')) cx
    let X[m] = Sx p0
    in if (n - m) == 0 then X else Lmx' m np1 amm cx x
  
  def iLmX' [nlat] (m: i64) (np1: i64) (amm: R.t) (cx: [nlat]R.t) (X: [np1]F.t): [nlat]F.t =
    let n = np1 - 1
    let x = tabulate nlat (\i -> F.zero)
    let m' = R.i64 m
    -- at each m we do x += X[m]P^_n
    let SX m x p= map2 (\xi pi -> xi F.+ (pi F.* X[m])) x p
    -- P^m_m
    let p0: [nlat]R.t = map (\cx -> R(amm * (r1 - cx * cx) ** (m' / r2) * (rm1) ** m')) cx
    let x[:] = SX m x p0
    -- P^m_(m + 1)
    let p1 = map2 (\cx p0 -> R((amn m' (m' + r1)) * cx * p0)) cx p0
    let x[:] = SX m x p1
    -- P^m_n -> P^m_n+1 -> P^m_n+2
    let p2 n p1 p0= map3 (\cx p1 p0 -> R((amn m' n) * cx * p1 + (bmn m' n) * p0)) cx p1 p0
    -- P^m_n
    let (x, pn, _) =
      match (n - m)
        case 0 -> (x, p0, p0)
        case 1 -> (x, p1, p0)
        case _ -> loop (x, p1, p0) = (x, p1, p0) for i < (n - m - 1) do
            let pi = p2 R(m' + r2 + i64 i) p1 p0
            let x[:] = SX (m + 2 + i) x pi
            in (x, pi, p1)
    in x
  
  def iLmX [nlat] (m: i64) (np1: i64) (amm: R.t) (cx: [nlat]R.t) (X: [np1]F.t): [nlat]F.t =
    let n = np1 - 1
    let x = tabulate nlat (\i -> F.zero)
    let m' = R.i64 m
    -- at each m we do x += X[m]P^_n
    let SX m x p= map2 (\xi pi -> xi F.+ (pi F.* X[m])) x p
    -- P^m_m
    let p0: [nlat]R.t = map (\cx -> R(amm * (r1 - cx * cx) ** (m' / r2) * (rm1) ** m')) cx
    let x[:] = SX m x p0
    in if (n - m) == 0 then x else iLmX' m np1 amm cx X
  
  def lt [np1] [nlon] [nlat] (amm: [np1]R.t) (cx: [nlat]R.t) (x: [nlon][nlat]F.t): [np1][np1]F.t =
    map2 (\m x -> Lmx m np1 amm[m] cx x) (iota np1) x[:np1] :> [np1][np1]F.t
  
  def ilt [np1] [nlon] [nlat] (amm: [np1]R.t) (cx: [nlat]R.t) (X: [np1][np1]F.t): [nlon][nlat]F.t =
    let out = tabulate_2d nlon nlat (\_ _ -> F.zero)
    let out[:np1] = map2 (\m x -> iLmX m np1 amm[m] cx x) (iota np1) X
    in out :> [nlon][nlat]F.t
  
  def bench (nxfm: i64) (lmax: i64) (nlat: i64) (nlon: i64): [nxfm][nlon][nlat]F.t =
    -- lmax > nlat
    let amm = all_amm lmax
    let x = tabulate_3d nxfm nlon nlat F.tab3
    let cx = lat_grid nlat
    let X = map (lt amm cx) x
    let x' = map (ilt amm cx) X
    in x'
}

module lts = mk_lt {mk_scalar_field f32}

-- ==
-- compiled input { 1i64 20i64 128i64 256i64 }
-- compiled input { 8i64 20i64 128i64 256i64 }
entry main (nxfm: i64) (lmax: i64) (nlat: i64) (nlon: i64) = lts.bench nxfm lmax nlat nlon