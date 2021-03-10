let main =
    let xs = [1i32,2,3,4,5,6,7,8] in
    map (\idx -> xs[idx-1] + xs[idx] + xs[idx+1]) (iota 8)