-- Must realise that the 'take (i+1)' is invariant to the 'map' after
-- interchange.
-- ==
-- structure gpu { Loop/SegRed 1 }

def main [n] (xs: [n]i32) =
  #[incremental_flattening(only_inner)]
  map (\x ->
         loop acc = x
         for i < n - 1 do
           #[unsafe]
           acc + i32.sum (take (i + 1) xs))
      xs
