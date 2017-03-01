-- Duplicate fields in a record type is an error.
-- ==
-- error: Duplicate

type t = {x: i32, x: i32}
