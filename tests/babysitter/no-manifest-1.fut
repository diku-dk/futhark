-- this is a simplified version of batch matrix inversion: it should not introduce a transposition for A[i,j]
-- ==
-- structure distributed {Manifest 0}

let gauss_jordan [nm] (n:i32) (m:i32) (A: *[nm]f32): [nm]f32 =
    loop A for i < n do
      -- the loop is outside the kernel, and hence `i` is a free
      -- variable in the kernel; hence fixing coalescing will likely
      -- do more work then the simple access: you will transpose an
      -- entire row to then read one element from it. This should not
      -- fire coalescing!
      let v1 = unsafe A[i]
      let A' = map (\ind -> let (k, j) = (ind / m, ind % m)
                            in if v1 == 0.0 then unsafe A[k*m+j] else
                            let x = unsafe (A[j] / v1) in
                                if k < n-1  -- Ap case
                                then unsafe ( A[(k+1)*m+j] - A[(k+1)*m+i] * x )
                                else x      -- irow case
                   ) (iota nm)
      in  scatter A (iota nm) A'

let mat_inv [n] (A: [n][n]f32): [n][n]f32 =
    let m = 2*n
    -- Pad the matrix with the identity matrix.
    let Ap = map (\ind -> let (i, j) = (ind / m, ind % m)
                          -- the innermost index `j` is variant to
                          -- the innermost kernel dimension `ind`;
                          -- hence "likely" already in coalesced form!
                          in  if j < n then unsafe ( A[i,j] )
                                       else if j == n+i
                                            then 1.0
                                            else 0.0
                 ) (iota (n*m))
    let Ap' = unflatten n m (gauss_jordan n m Ap)

    -- Drop the identity matrix at the front.
    in Ap'[0:n,n:n * 2] :> [n][n]f32

let main [m][n] (X : [m][n][n]f32) : [m][n][n]f32 =
  map mat_inv X
