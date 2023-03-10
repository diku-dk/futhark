import "a"
def g x = x + 2

def h x = f x


module m : { val x : i32 } = { def x = 2 }
def thing = m.x
