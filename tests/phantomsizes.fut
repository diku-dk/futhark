-- ==
-- input { [1,2,3] } output { [0,1,2] [1,2,3] }

type size [n] = [n]()

def size n = replicate n ()

def iota' [n] (_: size [n]) : [n]i32 =
  0..1..<i32.i64 n :> [n]i32

def length' [n] 'a (_: [n]a) : size [n] =
  size n

def f xs = zip (iota' (length' xs)) xs

def main (xs: []i32) = unzip (f xs)
