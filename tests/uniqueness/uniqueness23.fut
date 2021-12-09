-- Could fail after internalisation.

def consume (arr: *[](i32, i32)) = arr

def main (arr: *[](i32, i32)) =
  let arr' = rotate 1 arr
  in consume arr
