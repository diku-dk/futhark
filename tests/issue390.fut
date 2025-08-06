-- An error in the handling of reshape in the internaliser.  The
-- source-language array is of lesser rank than the corresponding
-- core-language array(s).

entry main n m (a: [n * m]([]i32, i32)) = unflatten a
