def secant_method (f: f32 -> f32) (a: f32) (b: f32) (tol: f32) =
  (loop (x_1, x_2, f_1, f_2) = (a, b, f a, f b)
   while (f32.abs (x_1 - x_2) > tol) do
     let x_0 = x_1 - f_1 * (x_1 - x_2) / (f_1 - f_2)
     let f_0 = f x_0
     in (x_0, x_1, f_0, f_1)).0

def minimum_D (stress: f32 -> f32) =
  let f D = stress D
  in secant_method f 0.01 0.1

def running (turning: bool) (R: f32) (v: f32) (a: f32) =
  let n = 20000i64
  let zs = tabulate n (\i -> f32.from_fraction i n)
  let maximum_stress D = D + 2
  let D = minimum_D maximum_stress
  in (zs, D)

def turning = running true 3 2 0
def main = turning.1
