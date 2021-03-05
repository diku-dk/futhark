-- Lambda cannot increase the dimensionality of the input (this is not a proper stencil).
-- Lambda must have primitive type as output.
--
-- ==
-- error: Lambda must return a primitive type or a tuple of primitive types

let main (xs:[]i32) = stencil_1d [-1,0,1] (\_ vs-> vs) (map (const ()) xs) xs
