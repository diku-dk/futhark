-- Composition of functions on enums.
-- ==
-- input { }
-- output { 3 }

type animal = #dog | #cat | #mouse | #bird
type planet = #mercury | #venus | #earth | #mars

def compose 'a 'b 'c (f: a -> b) (g: b -> c) : a -> c = \x -> g (f x)

def f (x: animal) : planet =
  match x
  case #dog -> #mercury
  case #cat -> #venus
  case #mouse -> #earth
  case #bird -> #mars

def g (x: planet) : i32 =
  match x
  case #mercury -> 1
  case #venus -> 2
  case #earth -> 3
  case #mars -> 4

def main : i32 = compose f g #mouse
