def pi = 3.141592653589793f32

def mpr_node [N] [T]
             (i: i64)
             (n: i64)
             (dt: f32)
             (nstep: i64)
             (i0: i64)
             (r: [N][T]f32)
             (V: [N][T]f32)
             (weights: [N][N]f32)
             (idelays: [N][N]i64)
             (G: f32)
             (I: f32)
             (Delta: f32)
             (eta: f32)
             (tau: f32)
             (J: f32) =
  let dr r V = 1 / tau * (Delta / (pi * tau) + 2 * V * r)
  let dV r V r_c = 1 / tau * (V ** 2 - pi ** 2 * tau ** 2 * r ** 2 + eta + J * tau * r + I + r_c)
  let r_bound r = if r >= 0f32 then r else 0f32
  let r_c = iota N |> map (\m -> weights[n, m] * r[m, i - idelays[n, m] - 1]) |> reduce (+) 0f32
  let r_c = r_c * G
  let dr_0 = dr r[n, i - 1] V[n, i - 1]
  let dV_0 = dV r[n, i - 1] V[n, i - 1] r_c
  let r_int = r[n, i - 1] + dt * dr_0
  let V_int = V[n, i - 1] + dt * dV_0
  let r_int = r_bound r_int
  in (r_int, V_int)

def mpr_integrate_seq [N] [T]
                      (dt: f32)
                      (nstep: i64)
                      (i0: i64)
                      (r: *[N][T]f32)
                      (V: *[N][T]f32)
                      (weights: [N][N]f32)
                      (idelays: [N][N]i64)
                      (G: f32)
                      (I: f32)
                      (Delta: f32)
                      (eta: f32)
                      (tau: f32)
                      (J: f32) : (*[N][T]f32, *[N][T]f32) =
  loop (r, V) for i_ < nstep do
    let n = 0
    let i = i_ + i0
    let (rn, Vn) = mpr_node i n dt nstep i0 r V weights idelays G I Delta eta tau J
    let r[n, i] = rn
    let V[n, i] = Vn
    in (r, V)

def sweep [N] [T]
          (g: i64)
          (dt: f32)
          (nstep: i64)
          (i0: i64)
          (r: [N][T]f32)
          (V: [N][T]f32)
          (weights: [N][N]f32)
          (idelays: [N][N]i64) =
  let Gs = tabulate g (\i -> 0.0 + (f32.i64 i) * 0.1)
  let do_one_seq G = mpr_integrate_seq dt nstep i0 (copy r) (copy V) weights idelays G 0.0f32 0.7f32 (-4.6f32) 1.0f32 14.5f32
  in map do_one_seq Gs

def main (ng: i64) (nh: i64) (nt: i64) (nn: i64) idelays weights r V =
  let dt = 0.01f32
  let f dt = sweep ng dt nt nh r V weights idelays
  let x = f dt
  in vjp f dt x
