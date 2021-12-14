-- Even though isnan32 is also the name of an intrinsic function, it's
-- not actually the same thing!

def isnan32 (x: i32) =
  let exponent = (x >> 23) & 0b11111111
  let significand = x & 0b11111111111111111111111
  in exponent == 0b11111111 && significand != 0

def main x = isnan32 x
