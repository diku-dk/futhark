-- Not OK because the module defines a higher-order type but the
-- module type specifies a zero-order type.
-- ==
-- error: non-lifted

module m = {type^ t = i32 -> i32}: {type t}
