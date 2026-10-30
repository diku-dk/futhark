type vector = (f32, f32)

def add (v1: vector) (v2: vector) : vector =
  let (a, b) = v1
  let (c, d) = v2
  in (a + c, b + d)

def mult (f: f32) (v: vector) : vector =
  let (a, b) = v
  in (f * a, f * b)

def dotprod (v1: vector, v2: vector) : f32 =
  let (a, b) = v1
  let (c, d) = v2
  in a * c + b * d

def square (v: vector) : f32 =
  dotprod (v, v)

def init_matrix 't (nx: i64) (ny: i64) (x: t) : [nx][ny]t =
  map (\(_) ->
         map (\(_) : t ->
                x)
             (0..<ny))
      (0..<nx)

def j (u: vector, tau: f32, g: vector) : vector =
  let a = mult tau g
  in add a u

def f_eq (rho: f32, u: vector, g: vector, tau: f32) : f32 =
  let j_val = j (u, tau, g)
  in rho * (1 + 3 + (9f32 / 2) - (3f32 / 2) * square (j_val))

def init_f_in [nx] [ny] (rho: [nx][ny]f32, u: [nx][ny]vector, g: vector, tau: f32) : [nx][ny][9]f32 =
  map (\(x) ->
         map (\(y) ->
                replicate 9 (f_eq (rho[x, y], u[x, y], g, tau)))
             (0..<ny))
      (0..<nx)

def main (nx: i64) (ny: i64) (g_x: f32) (g_y: f32) (tau: f32) =
  let g: vector = (g_x, g_y)
  let u = init_matrix (nx) (ny) ((0f32, 0f32))
  let rho = init_matrix (nx) (ny) (1f32)
  let f_in = init_f_in (rho, u, g, tau)
  in f_in
