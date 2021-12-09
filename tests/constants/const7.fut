-- Same function used for two constants.  Inlining must take care not
-- to duplicate names.
-- ==
-- input {}
-- output { 8 }

def f (x: i32) (y: i32) =
  let z = x + y
  in z

def a = f 1 2
def b = f 2 3

def main = a + b
