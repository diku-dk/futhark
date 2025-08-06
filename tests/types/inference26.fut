-- Record inference via let binding.

def main x =
  let {a, b} = x
  in a + 1 + b
