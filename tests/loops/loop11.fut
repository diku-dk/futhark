-- Tests a loop that simplification once messed up royally.
-- ==
-- input { 50 1.1f32 }
-- output { 14 }

def p (c: f32) : bool =
  c < 4.0

def f (x: f32, y: f32) : f32 =
  x * y

def main (depth: i32) (a: f32) : i32 =
  let (c, i) =
    loop (c, i) = (a, 0)
    while i < depth && p (c) do
      ( f (a, c)
      , i + 1
      )
  in i
