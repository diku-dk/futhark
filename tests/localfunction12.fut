-- Local function used in operator section.

def main (x: i32) (y: i32) (z: i32) =
  let add x y = x + y + z
  in x `add` y
