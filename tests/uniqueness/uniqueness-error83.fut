-- Consuming a tuple while also passing one of its components.
-- ==
-- error: Argument is consumed, but aliases

def g (a: *([]i32, []i32)) (b: []i32) : i32 =
  let a.0[0] = 10
  in a.0[0] + b[0]

def main (p: *([]i32, []i32)) : i32 = g p p.0
