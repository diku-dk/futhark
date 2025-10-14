-- Ascription of a functor result should be checked, even if the
-- functor is never instantiated.
-- ==
-- error: y

module F (S: {val x : i32}) : {val y : i32} = {def z : i32 = 2}

def main () : i32 = 0
