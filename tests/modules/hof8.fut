-- Lifted abstract types from a module parameter cannot be array
-- elements!
-- ==
-- error: might contain function

module m = \(p: {type^ a}) -> {def v : []p.a = []}
