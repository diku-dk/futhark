-- A plain expression in a record literal must be a record itself.
-- ==
-- error: record

val bad = {x=1, 2}
