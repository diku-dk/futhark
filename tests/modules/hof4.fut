-- FIXME: this _should_ be OK because we are allowed to narrow a type
-- in an ascription.
-- ==
-- error: functional

module m = { type^ t '^a = a } : { type t 'a }
