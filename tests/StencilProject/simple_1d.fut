let main =
	let xs : [8]i32 = [1,2,3,4,5,6,7,8]
        in stencil_1d [-1,0,1] (\_ xs -> xs[0]+xs[1]+xs[2] ) xs xs
