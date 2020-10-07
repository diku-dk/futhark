type complex = {r:f32, i:f32}
let complex r i : complex = {r, i}
let real r = complex r 0

let conjC (a:complex) : complex = {r = a.r, i = -a.i}

let addC (a:complex) (b:complex) : complex = {r=a.r+b.r, i=a.i+b.i}
let subC (a:complex) (b:complex) : complex = {r=a.r-b.r, i=a.i-b.i}
let mulC (a:complex) (b:complex) : complex = {r=a.r*b.r-a.i*b.i, i=a.r*b.i+a.i*b.r}

let pi:f32 = 3.141592653589793

let gfft [n] (inverse: bool) (xs:[n]complex) : [n]complex =
    let logN = assert (i64.popc n == 1) (i64.ctz n)
    let startTheta = pi * f32.from_fraction (2 - (i64.bool inverse << 2)) n
    let ms = n >> 1
    let iteration [l] ((xs:[l]complex), e, theta0) =
        let modc = (1 << e) - 1
        let xs' = tabulate l (\i ->
            let q = i & modc
            let p'= i >> e
            let p = p'>> 1
            let ai = q + (p << e)
            let bi = ai + ms
            let a = xs[ai]
            let b = xs[bi]
            let theta = theta0 * f32.i64 p
            in if bool.i64 (p' & 1)
                    then mulC (complex (f32.cos theta) (-f32.sin theta)) (subC a b)
                    else addC a b )
        in (xs', e + 1, theta0 * 2)
    in (iterate logN iteration (xs, 0, startTheta)).0

let gfft3 [m][n][k] inverse (A:[m][n][k]complex) =
    let A'   = tabulate_2d n k (\i j -> gfft inverse A[:,i,j])
    let A''  = tabulate_2d k m (\i j -> gfft inverse A'[:,i,j])
    let A''' = tabulate_2d m n (\i j -> gfft inverse A''[:,i,j])
    in A'''

let ifft3 [m][n][k] (x:[m][n][k]complex) =
    let f = real (f32.from_fraction 1 (m*n*k))
    in gfft3 true x |> map (map (map (mulC f)))

let main = map ifft3
