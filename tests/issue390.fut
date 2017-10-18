-- An error in the handling of reshape in the internaliser.  The
-- source-language array is of lesser rank than the corresponding
-- core-language array(s).

let main (a: []([]i32,i32)) = reshape (3,3) a
