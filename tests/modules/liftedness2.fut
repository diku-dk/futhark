-- ==
-- error: Non-lifted type abbreviations may not contain functions.

module type mt = {type t '^a}
module m : mt = {type t '^a = a}
