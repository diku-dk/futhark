-- From [Griewank 2008].
-- ==
-- entry: lighthouse_jvp lighthouse_vjp
-- compiled input { 2.0 1.5 0.4 2.1 }
-- output { 0.500000f64 -15.102799f64 75.415754f64 14.364905f64
--          0.500000f64 -21.987531f64 111.011046f64 21.144961f64
--        }

let lighthouse (nu, gamma, omega, t) =
  let y1 = (nu * f64.tan(omega * t)) / (gamma - f64.tan(omega * t))
  let y2 = (gamma * nu * f64.tan(omega * t)) / (gamma - f64.tan(omega * t))
  in (y1, y2)

entry lighthouse_jvp nu gamma omega t =
  let (y1_dnu, y2_dnu) =
    jvp lighthouse (nu, gamma, omega, t) (1, 0, 0, 0)
  let (y1_dgamma, y2_dgamma) =
    jvp lighthouse (nu, gamma, omega, t) (0, 1, 0, 0)
  let (y1_domega, y2_domega) =
    jvp lighthouse (nu, gamma, omega, t) (0, 0, 1, 0)
  let (y1_dt, y2_dt) =
    jvp lighthouse (nu, gamma, omega, t) (0, 0, 0, 1)
  in (y1_dnu, y1_dgamma, y1_domega, y1_dt,
      y2_dnu, y2_dgamma, y2_domega, y2_dt)

entry lighthouse_vjp nu gamma omega t =
  let (y1_dnu, y1_dgamma, y1_domega, y1_dt) =
    vjp lighthouse (nu, gamma, omega, t) (1, 0)
  let (y2_dnu, y2_dgamma, y2_domega, y2_dt) =
    vjp lighthouse (nu, gamma, omega, t) (0, 1)
  in (y1_dnu, y1_dgamma, y1_domega, y1_dt,
      y2_dnu, y2_dgamma, y2_domega, y2_dt)
