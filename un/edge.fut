module m1 = { def x = 2 }
module pm(m: {val x: i32}) = { def y = m.x }
module m2 = pm m1
entry main = m2.y
