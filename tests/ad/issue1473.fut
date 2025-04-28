-- test mpr sim with ad for params

def pi = 3.141592653589793f32

-- some type abbreviations
type mpr_pars = {G: f32, I: f32, Delta: f32, eta: f32, tau: f32, J: f32}
type mpr_node = (f32, f32)
type mpr_net [n] = [n]mpr_node

-- this is tranposed from mpr-pdq to avoid tranposes in history update
type mpr_hist [t] [n] = [t]mpr_net [n]
type connectome [n] = {weights: [n][n]f32, idelays: [n][n]i64}

-- do one time step w/ Euler
def mpr_step [t] [n] (now: i64) (dt: f32) (buf: *mpr_hist [t] [n]) (conn: connectome [n]) (p: mpr_pars) : *mpr_hist [t] [n] =
  -- define individual derivatives as in mpr pdq
  let dr r V = 1 / p.tau * (p.Delta / (pi * p.tau) + 2 * V * r)
  let dV r V r_c = 1 / p.tau * (V ** 2 - pi ** 2 * p.tau ** 2 * r ** 2 + p.eta + p.J * p.tau * r + p.I + r_c)
  let dfun (r, V, c) = (dr r V, dV r V c)
  -- unpack current state for clarity
  let (r, V) = last buf |> unzip
  -- connectivity eval
  let r_c_i i w d = map2 (\wj dj -> wj * buf[now - dj, i].0) w d |> reduce (+) 0f32 |> (* p.G)
  let r_c = map3 r_c_i (iota n) conn.weights conn.idelays
  -- Euler step
  let erV =
    map3 (\r V c -> (dr r V, dV r V c)) r V r_c
    |> map2 (\(r, V) (dr, dV) -> (r + dt * dr, V + dt * dV)) (last buf)
    |> map1 (\(r, V) -> (if r >= 0f32 then r else 0f32, V))
  -- now for the Heun step
  let (er, eV) = unzip erV
  let hrV =
    map3 (\r V c -> (dr r V, dV r V c)) er eV r_c
    |> map2 (\(r, V) (dr, dV) -> (r + dt * dr, V + dt * dV)) (last buf)
    |> map1 (\(r, V) -> (if r >= 0f32 then r else 0f32, V))
  -- return updated buffer
  in buf with [now + 1] = copy hrV

def run_mpr [t] [n] (horizon: i64) (dt: f32) (buf: mpr_hist [t] [n]) (conn: connectome [n]) (p: mpr_pars) : mpr_hist [t] [n] =
  loop buf = copy buf
  for now < (t - horizon - 1) do
    mpr_step (now + horizon) dt buf conn p

def mpr_pars_with_G (p: mpr_pars) (new_G: f32) : mpr_pars =
  let new_p = copy p
  in new_p with G = new_G

def loss [t] [n] (x: mpr_hist [t] [n]) : f32 =
  let r = map unzip x[t - 10:] |> unzip |> (.0)
  let sum = map (reduce (+) 0f32) r |> reduce (+) 0f32
  in sum

def sweep [t] [n] (ng: i64) (horizon: i64) (dt: f32) (buf: mpr_hist [t] [n]) (conn: connectome [n]) (p: mpr_pars) : [ng]f32 =
  let Gs = tabulate ng (\i -> 0.0 + (f32.i64 i) * 0.1)
  let do_one G = run_mpr horizon dt buf conn (mpr_pars_with_G p G) |> loss
  in map (\g -> vjp do_one g 1f32) Gs

-- ==
-- no_ispc input { 1i64 5i64 10i64 7i64 }
-- output { [0.000086f32] }
def main (ng: i64) (nh: i64) (nt: i64) (nn: i64) =
  let dt = 0.01f32
  let buf = tabulate_2d (nt + nh) nn (\i j -> (0.1f32, -2.0f32))
  let conn =
    { weights = tabulate_2d nn nn (\i j -> 0.1f32)
    , idelays = tabulate_2d nn nn (\i j -> ((i * j) % nh))
    }
  let p = {G = 0.1f32, I = 0.0f32, Delta = 0.7f32, eta = (-4.6f32), tau = 1.0f32, J = 14.5f32}
  in sweep ng nh dt buf conn p
