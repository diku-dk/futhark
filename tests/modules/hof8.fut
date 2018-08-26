-- Lifted abstract types from a module parameter cannot be array
-- elements!
-- ==
-- error: might be functional

module m = \(p: {type ^a}) -> { let v: []p.a = [] }
