-- ==
-- tags { no_webgpu }

def vecadd = map2 (f64.+)
def scale x v = map (x f64.*) v

def runge_kutta [n] (f: [n]f64 -> [n]f64) (yi: [n]f64) (tf: f64) (s: i64) =
  let h = tf / f64.i64 s
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

def an_ode_fun_vec [n] (x: [n]f64) (y: [n]f64) : [n]f64 =
  let f i x' y' = if i == 0 then 0 else x' * y'
  in map3 f (iota n) x (rotate 1 y)

def primal [n] (x: [n]f64) (s: i64) : []f64 =
  runge_kutta (an_ode_fun_vec x) (map (const 0) x) 2 s

entry gradient [n] (x: [n]f64) (s: i64) : []f64 =
  vjp (\x' -> last (primal x' s)) x 1
