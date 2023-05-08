-- a.fut
-- g, thing, m.x, k, thing, l and un are unused here.

def g x = x

def f x = x + 2i32

module m : { val x : i32 } = { def x = 2 }
module n : { module y : { val used_x : i32 } } = { module y : { val used_x : i32 } = { def used_x = 3 } }

def k = n.y.used_x
def thing = m.x
def l x = let un x = 1 in 1
