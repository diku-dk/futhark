-- Loop bound inference

let main x = (loop y = 2 for i < x do y*2, x + 1i8)
