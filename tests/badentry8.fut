-- It is OK to put the type annotations in a higher-order return type.
-- ==
-- warning: ^$

type t1 = {x: i32}
type t2 = t1

def main : t1 -> t2 = id
