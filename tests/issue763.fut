-- ==
-- tags { no_webgpu }

type vector = (f64, f64)

def add (v1: vector) (v2: vector) : vector =
  let (a, b) = v1
  let (c, d) = v2
  in (a + c, b + d)

def mult (f: f64) (v: vector) : vector =
  let (a, b) = v
  in (f * a, f * b)

def dotprod (v1: vector, v2: vector) : f64 =
  let (a, b) = v1
  let (c, d) = v2
  in a * c + b * d

def square (v: vector) : f64 =
  dotprod (v, v)

def init_matrix 't (nx: i64) (ny: i64) (x: t) : [nx][ny]t =
  map (\(_) ->
         map (\(_) : t ->
                x)
             (0..<ny))
      (0..<nx)

def j (u: vector, tau: f64, g: vector) : vector =
  let a = mult tau g
  in add a u

def f_eq (rho: f64, u: vector, g: vector, tau: f64) : f64 =
  let j_val = j (u, tau, g)
  in rho * (1 + 3 + (9f64 / 2) - (3f64 / 2) * square (j_val))

def init_f_in [nx] [ny] (rho: [nx][ny]f64, u: [nx][ny]vector, g: vector, tau: f64) : [nx][ny][9]f64 =
  map (\(x) ->
         map (\(y) ->
                replicate 9 (f_eq (rho[x, y], u[x, y], g, tau)))
             (0..<ny))
      (0..<nx)

def main (nx: i64) (ny: i64) (g_x: f64) (g_y: f64) (tau: f64) =
  let g: vector = (g_x, g_y)
  let u = init_matrix (nx) (ny) ((0f64, 0f64))
  let rho = init_matrix (nx) (ny) (1f64)
  let f_in = init_f_in (rho, u, g, tau)
  in f_in
