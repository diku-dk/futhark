-- A case of tiling with a complex dependency.
-- ==
-- compiled random input { [100]i32 } auto output
-- structure gpu { SegMap/Loop/SegMap 2 }

def main (xs: []i32) =
  map (\x ->
         let y = x + 2 -- Used in postlude.
         let z = loop z=0 while z < 1337 do z * 2 + y -- Uses 'y', but
                                                      -- cannot itself
                                                      -- be inlined in
                                                      -- the postlude.
         let loopres = #[sequential] i32.sum (map (+z) xs) -- Uses z.
         in loopres + i32.clz y -- 'y' must be made available here.
      )
      xs
