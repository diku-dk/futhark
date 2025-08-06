-- OK because the module defines a higher-order type and the module
-- type specifies a lifted type.

module m = {type^ t = i32 -> i32}: {type^ t}
