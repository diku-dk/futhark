-- #1687

type t = {a: [10]i32, b: bool}

def recUpdate (rec: *t) = rec.a with [0] = 1
