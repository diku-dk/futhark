-- Basic/naive use of ascription.
-- ==
-- input {} output { 2 }

module m = {def x = 2}
module m' : {val x : i32} = m

def main = m'.x
