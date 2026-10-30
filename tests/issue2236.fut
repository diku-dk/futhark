-- ==
-- tags { autodiff }

def vecadd = map2 (f32.+)
def scale x v = map (x f32.*) v

def runge_kutta [n] (f: [n]f32 -> [n]f32) (yi: [n]f32) (tf: f32) (s: i64) =
  let h = tf / f32.i64 s
  in loop yf = yi
     for _i < s do
       let k1 = f yf
       let k2 = f (yf `vecadd` scale (h / 2) k1)
       let k3 = f (yf `vecadd` scale (h / 2) k2)
       let k4 = f (yf `vecadd` scale h k3)
       in yf
          `vecadd` (scale (h / 6)
                          (k1
                           `vecadd` scale 2 k2
                           `vecadd` scale 2 k3
                           `vecadd` k4))

def an_ode_fun_vec [n] (x: [n]f32) (y: [n]f32) : [n]f32 =
  let f i x' y' = if i == 0 then 0 else x' * y'
  in map3 f (iota n) x (rotate 1 y)

def primal [n] (x: [n]f32) (s: i64) : []f32 =
  runge_kutta (an_ode_fun_vec x) (map (const 0) x) 2 s

entry gradient [n] (x: [n]f32) (s: i64) : []f32 =
  vjp (\x' -> last (primal x' s)) x 1
