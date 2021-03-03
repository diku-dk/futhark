
let main (xs:[]i32) = stencil_1d [-1,0,1] (\_ vs-> vs) (map (const ()) xs) xs
