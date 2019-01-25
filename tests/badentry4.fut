-- It is OK to have an array of opaques.
-- ==
-- warning: ^$

type opaque = {x:i32}

let main (x: i32): [1]opaque = [{x}]
