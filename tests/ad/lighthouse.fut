-- From [Griewank 2008].
-- ==
-- tags { autodiff }
-- entry: lighthouse_jvp lighthouse_vjp
-- input { 2.0f32 1.5f32 0.4f32 2.1f32 }
-- output { 2.902513633461043f32 -15.102798701184362f32 95.71780341846966f32 18.23196255589898f32
--          4.353770450191565f32 -16.849170784854458f32 143.57670512770449f32 27.347943833848472f32
--        }

def lighthouse (nu, gamma, omega, t) =
  let y1 = (nu * f32.tan (omega * t)) / (gamma - f32.tan (omega * t))
  let y2 = (gamma * nu * f32.tan (omega * t)) / (gamma - f32.tan (omega * t))
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
  in ( y1_dnu
     , y1_dgamma
     , y1_domega
     , y1_dt
     , y2_dnu
     , y2_dgamma
     , y2_domega
     , y2_dt
     )

entry lighthouse_vjp nu gamma omega t =
  let (y1_dnu, y1_dgamma, y1_domega, y1_dt) =
    vjp lighthouse (nu, gamma, omega, t) (1, 0)
  let (y2_dnu, y2_dgamma, y2_domega, y2_dt) =
    vjp lighthouse (nu, gamma, omega, t) (0, 1)
  in ( y1_dnu
     , y1_dgamma
     , y1_domega
     , y1_dt
     , y2_dnu
     , y2_dgamma
     , y2_domega
     , y2_dt
     )
