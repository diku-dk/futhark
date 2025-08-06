-- Refinement of a parametric may expand it.
-- ==

module type has_t = {type t 'a}
module type has_t' = has_t with t '^a = a
