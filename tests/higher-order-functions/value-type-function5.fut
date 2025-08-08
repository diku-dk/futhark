-- Lifted type parameters allows for the definition of general polymorphic
-- function composition. Without them, we are limited in which functions can be
-- composed.
-- ==
-- error: functional

def compose 'a 'b 'c (f: b -> c) (g: a -> b) : a -> c =
  \(x: a) -> f (g x)

def add (x: i32) (y: i32) : i32 = x + y
def double (x: i32) : i32 = x + x

def main (x: i32) (y: i32) : i32 =
  compose add double 3 5
