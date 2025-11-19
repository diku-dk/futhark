-- ==
-- entry: main
-- input { [0,1,2,3] }
-- output { [1,2,3,4] }

def automap 'a [n] 'x (f: a -> x) (as: [n]a): *[n]x = f as

entry main (x: []i32) = automap (+1) x
