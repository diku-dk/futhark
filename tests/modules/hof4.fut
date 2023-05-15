-- ==
-- error: non-lifted

module m = { type^ t '^a = a } : { type t 'a }
