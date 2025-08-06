-- Proper aliasing inference for function.

def f xs = map (+ 1i32) xs

def main (xss: *[][]i32) =
  let xss[0] = f xss[0]
  in xss
