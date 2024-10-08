-- Cannot refine a type twice.
-- ==
-- error: not an abstract type
module type mt = {type t}

module type mt' = mtwith t  = i64

module type mt'' = mt'with t  = i64