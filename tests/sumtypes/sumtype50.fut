type^ t = #foo (i32 -> i32) | #bar i32

def use_p (p: t) =
  match p
  case #foo f -> f 2
  case #bar x -> x

def main =
  (use_p (#foo (+ 1)), use_p (#bar 1))
