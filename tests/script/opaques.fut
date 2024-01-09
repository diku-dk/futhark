-- ==
-- entry: bar
-- script input { foo 10 }

type bools [n] = #foo [n]bool

entry foo (n: i64) : {x:[n]bool,y:bool} = {x=replicate n true,y=false}
entry bar [m] (b: {x:[m]bool,y:bool}) : bool = b.y
