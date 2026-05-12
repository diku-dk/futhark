-- Allow to access argument field as size for return type
-- ==

def f (p: {a: i64, b: bool}) : [p.a]i64 = iota p.a
