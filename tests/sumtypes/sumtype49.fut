-- Based on #1917
-- ==
-- input {} output { 3 2 }

type^ t = #foo (i32 -> i32) | #bar (f32 -> i32)

def use_p (p: t) =
  match p
  case #foo f -> f 2
  case #bar f -> f 2

def main = (use_p (#foo (+ 1)), use_p (#bar i32.f32))
