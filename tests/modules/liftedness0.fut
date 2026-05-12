-- Abstract type must be at most as lifted as in the module type.
-- ==
-- error: vector

module type mt = {type~ vector}

module m = {type^ vector = []i32}: mt
