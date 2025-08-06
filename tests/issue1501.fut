def effective_stress (d: f32) (_F_x, _F_y, F_z) = 0

def running (turning: bool) (R: f32) (v: f32) (a: f32) =
  let n = 20000
  let zs = tabulate n (\i -> f32.from_fraction i n)
  let (Fs, Ms) = unzip (map (\_ -> ((0, 0, 0), (0, 0, 0))) zs)
  in (zs, unzip3 Fs, unzip3 Ms)

def straight = running false 0 0 0
def zs_str = straight.0
def uczip3 (a, b, c) = zip3 a b c
def stress Fs Ms = map2 effective_stress zs_str (uczip3 Fs)
entry stress_str = stress (straight.1) (straight.2)
