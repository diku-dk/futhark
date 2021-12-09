-- An error in the handling of reshape in the internaliser.  The
-- source-language array is of lesser rank than the corresponding
-- core-language array(s).

def main (a: []([]i32,i32)) = unflatten 3 3 a
