-- Currently fails
-- == 
-- input { [5i64,7i64] [0i64,1i64] }
-- auto output
def main ns is = map2 (\n (i:i64) -> let is = iota n
                                     let xs = map (+2) is
                                    --  let ts = scan (*) 1 xs
                                     let ys = map (*i) is
                                     let zs = scan (+) 0 ys
                                     in (i64.sum xs, (opaque zs)[i]))
                      ns is
                 |> unzip
