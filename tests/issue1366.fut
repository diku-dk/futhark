#[noinline]
def update [n] (xs: [n]i32) : [n]i32 =
  map (+ 1) xs

def main [n] [m] (xss: *[m][n]i32) =
  #[unsafe]
  loop xss for i < m do
    copy xss with [i] = update xss[i]
