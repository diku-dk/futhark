-- Overloaded numbers should not track aliases.

def main =
  let arr = [3, 7]
  let a = arr[0]
  let arr[0] = 0
  in a
