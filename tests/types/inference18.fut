-- Currying overloaded operators.

def eq1 = (== 1)
def eq2 = (==) : (i32 -> i32 -> bool)
def add1 = (+ 1)
def add2 = (+) : (i32 -> i32 -> i32)

def main (x: i32) = eq1 x && eq2 (add1 x) (add2 x x)
