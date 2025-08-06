-- Not OK, because the module type specifies a more liberal type than
-- defined by the module.
-- ==
-- error: Module type

module m = {def f 'a (x: a) = ([x])[0]}: {val f '^a : a -> a}

def main = m.f id 0i32
