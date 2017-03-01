-- Duplicate fields in a record expression is an error.
-- ==
-- error: Duplicate

val bad = {x=1, x=2}
